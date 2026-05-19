"""
download_crw_hpc.py

Download NOAA Coral Reef Watch (CRW) 5km gridded data from the NOAA FTP server.
Supports SST and DHW products at daily, monthly, and annual temporal resolutions.

Features:
  - Parallel downloads via ThreadPoolExecutor
  - Per-file retry with exponential backoff (resilient to dropped connections)
  - Atomic write (temp file -> rename) to avoid corrupt partials on disk
  - MD5 checksum verification against NOAA-provided .md5 sidecar files
  - Three-state file handling: verified OK / failed checksum (re-download) / missing

Usage examples:
  # SST daily (default) for 2024-2025, 4 workers
  python download_crw_hpc.py --years 2024 2025

  # DHW daily for 2020-2025, 8 workers
  python download_crw_hpc.py --measure dhw --years 2020 2025 --workers 8

  # DHW monthly for all available years
  python download_crw_hpc.py --measure dhw --resolution monthly --years $(seq 1985 2026)

  # DHW annual for all available years
  python download_crw_hpc.py --measure dhw --resolution annual --years $(seq 1985 2026)

  # SST annual (downloads both sst-max and sst-mean variants)
  python download_crw_hpc.py --measure sst --resolution annual --years $(seq 1985 2026)
"""

from ftplib import FTP, error_perm, error_temp
from pathlib import Path
import argparse
import hashlib
import io
import time

from concurrent.futures import ThreadPoolExecutor, as_completed

# ---------------------------------------------------------------------------
# FTP server and remote path configuration
# ---------------------------------------------------------------------------

FTP_HOST = "ftp.star.nesdis.noaa.gov"

# Remote base paths keyed by (measure, resolution).
#
# Annual notes:
#   - All annual products share one flat directory (no year subdirs): .../annual/
#   - DHW annual:  ct5km_dhw-max_v3.1_YYYY.nc
#   - SST annual:  ct5km_sst-max_v3.1_YYYY.nc  and  ct5km_sst-mean_v3.1_YYYY.nc
#   - Files are filtered from the shared listing by product prefix + year.
ANNUAL_REMOTE_DIR = "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/annual"

REMOTE_PATHS = {
    ("sst", "daily"):   "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/sst",
    ("sst", "monthly"): "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/monthly/sst",
    ("sst", "annual"):  ANNUAL_REMOTE_DIR,
    ("dhw", "daily"):   "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/dhw",
    ("dhw", "monthly"): "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/monthly/dhw",
    ("dhw", "annual"):  ANNUAL_REMOTE_DIR,
}

# Resolutions that store all years in one flat directory (no per-year subdirs).
FLAT_LAYOUT = {
    ("sst", "annual"),
    ("dhw", "annual"),
}

# Filename prefixes used to match files in the shared flat annual directory.
# Matching is done against the bare filename (not the full remote path).
# SST has two annual variants; DHW has one.
ANNUAL_PREFIXES = {
    "sst": ["ct5km_sst-max_v3.1_", "ct5km_sst-mean_v3.1_"],
    "dhw": ["ct5km_dhw-max_v3.1_"],
}

# Local root — edit to match your storage layout.
LOCAL_BASE = Path("/QRISdata/Q5947/storage1tb/temperature_data")

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

def parse_args():
    parser = argparse.ArgumentParser(
        description="Download NOAA CRW SST or DHW data from the NOAA FTP server.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--measure",
        choices=["sst", "dhw"],
        default="sst",
        help="Data product to download: 'sst' (default) or 'dhw'.",
    )
    parser.add_argument(
        "--resolution",
        choices=["daily", "monthly", "annual"],
        default="daily",
        help="Temporal resolution: 'daily' (default), 'monthly', or 'annual'.",
    )
    parser.add_argument(
        "--years",
        nargs="+",
        type=int,
        required=True,
        help="One or more years to download (e.g. --years 2023 2024 2025).",
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=4,
        help="Number of parallel download workers (default: 4).",
    )
    parser.add_argument(
        "--retries",
        type=int,
        default=5,
        help="Max download attempts per file before giving up (default: 5).",
    )
    parser.add_argument(
        "--retry-delay",
        type=float,
        default=2.0,
        help="Initial retry back-off delay in seconds; doubles each attempt (default: 2).",
    )
    parser.add_argument(
        "--no-checksum",
        action="store_true",
        default=False,
        help="Skip MD5 checksum verification (faster, but no integrity check).",
    )
    return parser.parse_args()


# ---------------------------------------------------------------------------
# FTP helpers
# ---------------------------------------------------------------------------

def ftp_connect(remote_dir):
    """Open a fresh, logged-in FTP connection and CWD into remote_dir."""
    ftp = FTP(FTP_HOST, timeout=60)
    ftp.login()
    ftp.cwd(remote_dir)
    return ftp


def list_remote_files(remote_dir, retries=3, delay=2.0):
    """
    Return bare filenames (not full paths) for all entries in remote_dir.

    ftp.nlst() may return full remote paths or bare names depending on the
    server. Path(entry).name normalises both to just the filename.
    Retries on transient errors with exponential back-off.
    """
    for attempt in range(1, retries + 1):
        try:
            ftp = ftp_connect(remote_dir)
            entries = ftp.nlst()
            ftp.quit()
            return [Path(e).name for e in entries]
        except (error_temp, ConnectionError, OSError) as e:
            if attempt == retries:
                raise
            wait = delay * (2 ** (attempt - 1))
            print(f"  [listing] Attempt {attempt} failed ({e}). Retrying in {wait:.0f}s ...")
            time.sleep(wait)
    return []


def fetch_remote_md5(filename, remote_dir, retries=3, delay=2.0):
    """
    Fetch the NOAA-provided .md5 sidecar file for filename and return the
    expected hex digest string, or None if the sidecar does not exist or
    cannot be parsed.

    NOAA .md5 files contain a single line in one of two formats:
      MD5 (ct5km_dhw-max_v3.1_1985.nc) = d41d8cd98f00b204e9800998ecf8427e
      d41d8cd98f00b204e9800998ecf8427e  ct5km_dhw-max_v3.1_1985.nc
    Both are handled below.
    """
    md5_filename = filename + ".md5"
    for attempt in range(1, retries + 1):
        try:
            ftp = ftp_connect(remote_dir)
            buf = io.BytesIO()
            ftp.retrbinary(f"RETR {md5_filename}", buf.write)
            ftp.quit()
            content = buf.getvalue().decode().strip()
            # BSD format: MD5 (name) = hash
            if "=" in content:
                return content.split("=")[-1].strip().lower()
            # GNU format: hash  name
            return content.split()[0].strip().lower()
        except error_perm:
            # No .md5 sidecar exists for this file
            return None
        except (error_temp, ConnectionError, OSError) as e:
            if attempt == retries:
                return None
            wait = delay * (2 ** (attempt - 1))
            time.sleep(wait)
    return None


def md5_of_file(path):
    """Return the MD5 hex digest of a local file."""
    h = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


# ---------------------------------------------------------------------------
# Per-file download with checksum verification + retry + atomic write
# ---------------------------------------------------------------------------

def download_file(filename, remote_dir, local_dir, retries, initial_delay, verify_checksum):
    """
    Download a single file from remote_dir into local_dir.

    File states:
      SKIP   — file exists locally and passes MD5 verification (or --no-checksum)
      RECHECK— file exists but failed MD5; deleted and re-downloaded
      OK     — file freshly downloaded and verified
      ERROR  — permanent FTP error (404 etc.), no retry
      FAIL   — exhausted retries on transient errors

    Atomic write: data goes to filename.part first; only renamed to the final
    name on success, so a crash never leaves a corrupt file that would be
    silently skipped on the next run.
    """
    final_path = local_dir / filename
    part_path  = local_dir / (filename + ".part")
    recheck    = False

    # --- Check existing file ---
    if final_path.exists() and final_path.stat().st_size > 0:
        if not verify_checksum:
            return f"  SKIP   {filename}  (no checksum)"
        expected = fetch_remote_md5(filename, remote_dir)
        if expected is None:
            # No sidecar available — trust the file
            return f"  SKIP   {filename}  (no .md5 sidecar, assuming OK)"
        actual = md5_of_file(final_path)
        if actual == expected:
            return f"  SKIP   {filename}  (MD5 verified)"
        # Checksum mismatch — delete and re-download
        print(f"  RECHECK {filename}: MD5 mismatch (local={actual[:8]}... expected={expected[:8]}...) — re-downloading")
        final_path.unlink()
        recheck = True

    # Clean up any leftover .part file
    if part_path.exists():
        part_path.unlink()

    last_error = None
    prefix = "RECHECK" if recheck else "OK"

    for attempt in range(1, retries + 1):
        try:
            ftp = ftp_connect(remote_dir)
            with open(part_path, "wb") as f:
                ftp.retrbinary(f"RETR {filename}", f.write)
            ftp.quit()

            # Verify checksum of freshly downloaded file before committing
            if verify_checksum:
                expected = fetch_remote_md5(filename, remote_dir)
                if expected is not None:
                    actual = md5_of_file(part_path)
                    if actual != expected:
                        part_path.unlink()
                        last_error = f"MD5 mismatch after download (got {actual[:8]}..., expected {expected[:8]}...)"
                        if attempt < retries:
                            wait = initial_delay * (2 ** (attempt - 1))
                            print(f"  RETRY  {filename} (attempt {attempt}/{retries}, "
                                  f"waiting {wait:.0f}s): {last_error}")
                            time.sleep(wait)
                        continue

            part_path.rename(final_path)
            return f"  {prefix:<7} {filename}  ->  {final_path}"

        except error_perm as e:
            if part_path.exists():
                part_path.unlink()
            return f"  ERROR  {filename}: permanent FTP error — {e}"

        except Exception as e:
            last_error = e
            if part_path.exists():
                part_path.unlink()
            if attempt < retries:
                wait = initial_delay * (2 ** (attempt - 1))
                print(f"  RETRY  {filename} (attempt {attempt}/{retries}, "
                      f"waiting {wait:.0f}s): {e}")
                time.sleep(wait)

    return f"  FAIL   {filename} after {retries} attempts — {last_error}"


# ---------------------------------------------------------------------------
# Main download orchestration
# ---------------------------------------------------------------------------

def download_crw(measure, resolution, years, workers, retries, retry_delay, verify_checksum):
    key = (measure, resolution)
    if key not in REMOTE_PATHS:
        raise ValueError(f"Unsupported (measure, resolution) combination: {key}")

    base_remote = REMOTE_PATHS[key]
    local_base  = LOCAL_BASE / measure / resolution
    local_base.mkdir(parents=True, exist_ok=True)

    print(f"Measure      : {measure.upper()}")
    print(f"Resolution   : {resolution}")
    print(f"Years        : {sorted(years)}")
    print(f"Workers      : {workers}")
    print(f"Retries/file : {retries}")
    print(f"Checksum     : {'enabled (MD5)' if verify_checksum else 'disabled (--no-checksum)'}")
    print(f"Remote root  : {base_remote}")
    print(f"Local root   : {local_base}")
    print("-" * 60)

    flat = key in FLAT_LAYOUT

    if flat:
        print(f"\nFlat layout — listing {base_remote} once for all years ...")
        try:
            all_names = list_remote_files(base_remote)
        except Exception as e:
            print(f"Could not list {base_remote}: {e} — aborting.")
            return
        all_nc = [n for n in all_names if n.endswith(".nc")]
        print(f"Found {len(all_nc)} .nc files in flat directory.")
        if all_nc:
            print(f"  Sample filenames: {all_nc[:3]}")

    for year in sorted(years):
        year_str       = str(year)
        local_year_dir = local_base / year_str
        local_year_dir.mkdir(parents=True, exist_ok=True)

        if flat:
            prefixes = ANNUAL_PREFIXES[measure]
            nc_files = [
                n for n in all_nc
                if any(n.startswith(p) for p in prefixes) and year_str in n
            ]
            remote_dir = base_remote
            print(f"\n[{year}] {len(nc_files)} file(s) matched  |  saving to {local_year_dir}")
        else:
            remote_dir = f"{base_remote}/{year_str}"
            print(f"\n[{year}] Listing {remote_dir} ...")
            try:
                names    = list_remote_files(remote_dir)
                nc_files = [n for n in names if n.endswith(".nc")]
            except Exception as e:
                print(f"[{year}] Could not list remote directory: {e} — skipping.")
                continue
            print(f"[{year}] {len(nc_files)} .nc file(s) found  |  saving to {local_year_dir}")

        if not nc_files:
            print(f"[{year}] Nothing to download — skipping.")
            continue

        print(f"[{year}] Starting downloads with {workers} workers ...")
        with ThreadPoolExecutor(max_workers=workers) as executor:
            futures = {
                executor.submit(
                    download_file,
                    name,
                    remote_dir,
                    local_year_dir,
                    retries,
                    retry_delay,
                    verify_checksum,
                ): name
                for name in nc_files
            }
            ok = skip = fail = recheck = 0
            for future in as_completed(futures):
                result = future.result()
                print(result)
                tag = result.strip().split()[0].upper()
                if tag == "OK":
                    ok += 1
                elif tag == "SKIP":
                    skip += 1
                elif tag == "RECHECK":
                    recheck += 1
                else:
                    fail += 1

        print(
            f"[{year}] Done — {ok} downloaded, {recheck} re-downloaded (bad checksum), "
            f"{skip} skipped (verified), {fail} failed."
        )

    print("\nAll years processed.")


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    args = parse_args()
    download_crw(
        measure          = args.measure,
        resolution       = args.resolution,
        years            = args.years,
        workers          = args.workers,
        retries          = args.retries,
        retry_delay      = args.retry_delay,
        verify_checksum  = not args.no_checksum,
    )