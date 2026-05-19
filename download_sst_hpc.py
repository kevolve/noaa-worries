"""
download_crw_hpc.py

Download NOAA Coral Reef Watch (CRW) 5km gridded data from the NOAA FTP server.
Supports SST and DHW products at daily, monthly, and annual temporal resolutions.

Features:
  - Parallel downloads via ThreadPoolExecutor
  - Per-file retry with exponential backoff (resilient to dropped connections)
  - Skip-if-exists + atomic write (temp file → rename) to avoid corrupt partials
  - Separate local directory trees per measure and resolution

Usage examples:
  # SST daily (default) for 2024–2025, 4 workers
  python download_crw_hpc.py --years 2024 2025

  # DHW daily for 2020–2025, 8 workers
  python download_crw_hpc.py --measure dhw --years 2020 2025 --workers 8

  # DHW monthly for all available years
  python download_crw_hpc.py --measure dhw --resolution monthly --years $(seq 1985 2026)

  # DHW annual for all available years
  python download_crw_hpc.py --measure dhw --resolution annual --years $(seq 1985 2026)

  # SST + DHW daily, all years (run twice with different --measure flags):
  python download_crw_hpc.py --measure sst --years {1985..2026} --workers 8
  python download_crw_hpc.py --measure dhw --years {1985..2026} --workers 8
"""

from ftplib import FTP, error_perm, error_temp
from pathlib import Path
import argparse
import time
import os

from concurrent.futures import ThreadPoolExecutor, as_completed

# ---------------------------------------------------------------------------
# FTP server and remote path configuration
# ---------------------------------------------------------------------------

FTP_HOST = "ftp.star.nesdis.noaa.gov"

# Remote base paths keyed by (measure, resolution).
# DHW annual files live under 'yearly' on the NOAA server (not 'annual').
REMOTE_PATHS = {
    ("sst", "daily"):   "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/sst",
    ("sst", "monthly"): "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/monthly/sst",
    ("sst", "annual"):  "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/yearly/sst",
    ("dhw", "daily"):   "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/dhw",
    ("dhw", "monthly"): "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/monthly/dhw",
    ("dhw", "annual"):  "/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/yearly/dhw",
}

# Local base paths — edit to match your storage layout
LOCAL_BASE = Path("/QRISdata/Q5947/storage1tb/data")

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
        help="Number of parallel download workers (default: 4). "
             "Be mindful of NOAA server connection limits.",
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
    return parser.parse_args()


# ---------------------------------------------------------------------------
# FTP helpers
# ---------------------------------------------------------------------------

def ftp_connect(remote_dir: str) -> FTP:
    """Open a fresh, logged-in FTP connection and CWD into remote_dir."""
    ftp = FTP(FTP_HOST, timeout=60)
    ftp.login()
    ftp.cwd(remote_dir)
    return ftp


def list_remote_files(remote_dir: str, retries: int = 3, delay: float = 2.0) -> list[str]:
    """
    Return the list of filenames in remote_dir.
    Retries on transient errors with exponential back-off.
    """
    for attempt in range(1, retries + 1):
        try:
            ftp = ftp_connect(remote_dir)
            files = ftp.nlst()
            ftp.quit()
            return files
        except (error_temp, ConnectionError, OSError) as e:
            if attempt == retries:
                raise
            wait = delay * (2 ** (attempt - 1))
            print(f"  [listing] Attempt {attempt} failed ({e}). Retrying in {wait:.0f}s …")
            time.sleep(wait)
    return []   # unreachable, but satisfies type checkers


# ---------------------------------------------------------------------------
# Per-file download with retry + atomic write
# ---------------------------------------------------------------------------

def download_file(
    filename: str,
    remote_dir: str,
    local_dir: Path,
    retries: int,
    initial_delay: float,
) -> str:
    """
    Download a single file from remote_dir into local_dir.

    Safety features:
      • Skip if the file already exists and is non-empty.
      • Write to a .part temp file; rename to final name only on success.
        This means an interrupted download never leaves a corrupt file that
        would be silently skipped on the next run.
      • Retry up to `retries` times with exponential back-off.
    """
    final_path = local_dir / filename
    part_path  = local_dir / (filename + ".part")

    # Skip completed files
    if final_path.exists() and final_path.stat().st_size > 0:
        return f"  SKIP  {filename}"

    # Remove any leftover .part file from a previous interrupted attempt
    if part_path.exists():
        part_path.unlink()

    last_error: Exception | None = None

    for attempt in range(1, retries + 1):
        try:
            ftp = ftp_connect(remote_dir)
            with open(part_path, "wb") as f:
                ftp.retrbinary(f"RETR {filename}", f.write)
            ftp.quit()

            # Atomic rename: only reaches here if the download succeeded
            part_path.rename(final_path)
            return f"  OK    {filename}"

        except (error_perm,) as e:
            # Permanent FTP error (e.g. file not found) — no point retrying
            if part_path.exists():
                part_path.unlink()
            return f"  ERROR {filename}: permanent FTP error — {e}"

        except Exception as e:
            last_error = e
            if part_path.exists():
                part_path.unlink()
            if attempt < retries:
                wait = initial_delay * (2 ** (attempt - 1))
                # Note: print from threads is fine; lines may interleave but
                # each print() call is atomic enough for progress monitoring.
                print(f"  RETRY {filename} (attempt {attempt}/{retries}, waiting {wait:.0f}s): {e}")
                time.sleep(wait)

    return f"  FAIL  {filename} after {retries} attempts — {last_error}"


# ---------------------------------------------------------------------------
# Main download orchestration
# ---------------------------------------------------------------------------

def download_crw(
    measure: str,
    resolution: str,
    years: list[int],
    workers: int,
    retries: int,
    retry_delay: float,
) -> None:
    key = (measure, resolution)
    if key not in REMOTE_PATHS:
        raise ValueError(f"Unsupported (measure, resolution) combination: {key}")

    base_remote = REMOTE_PATHS[key]
    local_base  = LOCAL_BASE / measure / resolution
    local_base.mkdir(parents=True, exist_ok=True)

    print(f"Measure     : {measure.upper()}")
    print(f"Resolution  : {resolution}")
    print(f"Years       : {years}")
    print(f"Workers     : {workers}")
    print(f"Retries/file: {retries}")
    print(f"Local root  : {local_base}")
    print("-" * 60)

    for year in sorted(years):
        year_str        = str(year)
        remote_year_dir = f"{base_remote}/{year_str}"
        local_year_dir  = local_base / year_str
        local_year_dir.mkdir(parents=True, exist_ok=True)

        print(f"\n[{year}] Listing {remote_year_dir} …")
        try:
            files = list_remote_files(remote_year_dir)
        except Exception as e:
            print(f"[{year}] Could not list remote directory: {e} — skipping year.")
            continue

        # Filter to only .nc files (skip hidden files / index listings)
        nc_files = [f for f in files if f.endswith(".nc")]
        print(f"[{year}] {len(nc_files)} .nc files found — downloading with {workers} workers …")

        with ThreadPoolExecutor(max_workers=workers) as executor:
            futures = {
                executor.submit(
                    download_file,
                    f,
                    remote_year_dir,
                    local_year_dir,
                    retries,
                    retry_delay,
                ): f
                for f in nc_files
            }
            ok = skip = fail = 0
            for future in as_completed(futures):
                result = future.result()
                print(result)
                if result.startswith("  OK"):
                    ok += 1
                elif result.startswith("  SKIP"):
                    skip += 1
                else:
                    fail += 1

        print(
            f"[{year}] Done — {ok} downloaded, {skip} skipped (already exist), "
            f"{fail} failed."
        )

    print("\nAll years processed.")


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    args = parse_args()
    download_crw(
        measure     = args.measure,
        resolution  = args.resolution,
        years       = args.years,
        workers     = args.workers,
        retries     = args.retries,
        retry_delay = args.retry_delay,
    )