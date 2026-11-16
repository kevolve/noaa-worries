#!/usr/bin/env python3
"""
crw_archive.py - local parquet archive of NOAA Coral Reef Watch 5 km products.

Each NOAA NetCDF file becomes one parquet file holding ocean cells only:

    date     date32   date of the layer (first day for monthly/annual files)
    cell_id  int32    row-major index on the global 0.05 degree grid
    <value>  int16    packed integer, e.g. sst_x100 = SST (deg C) * 100

Grid:  ix = cell_id % 7200, iy = cell_id // 7200
       lon = -179.975 + 0.05 * ix,  lat = 89.975 - 0.05 * iy

Files are zstd-compressed with DELTA_BINARY_PACKED integer encoding.

Commands (run with -h for options):
    check     verify Python packages, data root and (optionally) the server
    update    list the server, download new files, verify md5, convert
    ingest    convert NetCDF files already on disk
    extract   build a regional subset (e.g. gbr) sorted for time series
    status    summarise the archive

Data root: --root, else $CRW_ROOT, else the first existing path under
"roots" in crw_config.json (same folder as this script).
"""
from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import importlib
import io
import json
import os
import re
import sys
import time
from concurrent.futures import ProcessPoolExecutor, as_completed
from ftplib import FTP, error_perm
from pathlib import Path

REQUIRED = {"numpy": "1.20", "netCDF4": "1.5", "pyarrow": "12.0"}
SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_CONFIG = SCRIPT_DIR / "crw_config.json"

NLON, NLAT, RES = 7200, 3600, 0.05
LON0, LAT0 = -179.975, 89.975          # centre of cell ix = 0 / iy = 0
ZSTD_LEVEL = 9
ROW_GROUP = 1 << 20
TIMEFRAMES = ("daily", "monthly", "annual")
SKIP_VARS = {"mask", "sea_ice_fraction", "crs", "time", "lat", "lon"}
FILE_RE = re.compile(
    r"^(?:coraltemp|ct5km_(?P<prod>[a-z0-9-]+))_v3\.1_(?P<date>\d{4}|\d{6}|\d{8})\.nc$"
)


# --------------------------------------------------------------------------
# Environment checks
# --------------------------------------------------------------------------

def _vtuple(v):
    return tuple(int(x) for x in re.findall(r"\d+", v)[:3])


def check_packages(verbose=False):
    """Return True when every required package imports at a usable version."""
    ok = True
    for name, minv in REQUIRED.items():
        try:
            ver = getattr(importlib.import_module(name), "__version__", "0")
            good = _vtuple(ver) >= _vtuple(minv)
            msg = f"{name} {ver}" + ("" if good else f"  (need >= {minv})")
        except Exception as e:  # ImportError or a broken shared library
            good, msg = False, f"{name} not importable: {e}"
        ok &= good
        if verbose or not good:
            print(f"  [{'ok' if good else 'MISSING'}] {msg}")
    return ok


def require_packages():
    if check_packages():
        return
    req = SCRIPT_DIR / "requirements.txt"
    sys.exit(
        "\nRequired Python packages are missing (listed above).\n"
        f"  Interpreter: {sys.executable}\n"
        "  Fix: load/activate the intended Python environment, then run\n"
        f"       {sys.executable} -m pip install --user -r {req}\n"
    )


# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

def load_config(path=None):
    path = Path(path or os.environ.get("CRW_CONFIG") or DEFAULT_CONFIG)
    with open(path) as f:
        return json.load(f)


def resolve_root(cfg, root=None):
    if root:
        return Path(root)
    if os.environ.get("CRW_ROOT"):
        return Path(os.environ["CRW_ROOT"])
    for name, p in cfg.get("roots", {}).items():
        if Path(p).is_dir():
            return Path(p)
    sys.exit("No data root found. Use --root, set CRW_ROOT, or edit 'roots' in "
             f"{DEFAULT_CONFIG}.")


def parse_years(spec):
    if not spec:
        return list(range(1985, dt.date.today().year + 1))
    years = []
    for tok in spec:
        for part in str(tok).split(","):
            if "-" in part:
                a, b = part.split("-")
                years += range(int(a), int(b) + 1)
            elif part:
                years.append(int(part))
    return sorted(set(years))


# --------------------------------------------------------------------------
# File naming
# --------------------------------------------------------------------------

def parse_name(filename):
    """NOAA filename -> (product, timeframe, date) or None."""
    m = FILE_RE.match(filename)
    if not m:
        return None
    d = m.group("date")
    if len(d) == 8:
        tf, date = "daily", dt.date(int(d[:4]), int(d[4:6]), int(d[6:]))
    elif len(d) == 6:
        tf, date = "monthly", dt.date(int(d[:4]), int(d[4:]), 1)
    else:
        tf, date = "annual", dt.date(int(d), 1, 1)
    return (m.group("prod") or "sst"), tf, date


def date_tag(tf, date):
    return date.strftime({"daily": "%Y%m%d", "monthly": "%Y%m", "annual": "%Y"}[tf])


def parquet_path(root, product, tf, date):
    name = f"{product}_{date_tag(tf, date)}.parquet"
    base = Path(root) / tf / product
    return base / name if tf == "annual" else base / f"year={date.year}" / name


def product_matches(product, wanted, tf):
    """Daily: exact name. Monthly/annual: 'sst' also matches 'sst-mean' etc."""
    if product == wanted:
        return True
    if tf == "daily":
        return False
    return product.startswith(wanted + "-") and product[len(wanted) + 1:] in ("mean", "min", "max")


# --------------------------------------------------------------------------
# NetCDF -> parquet
# --------------------------------------------------------------------------

def _oriented(var, ds):
    """Return the 2-D raw array as [north -> south, west -> east]."""
    import numpy as np
    var.set_auto_maskandscale(False)
    raw = np.squeeze(var[:])
    if raw.shape != (NLAT, NLON):
        raise ValueError(f"unexpected grid shape {raw.shape} for {var.name}")
    lat = np.asarray(ds.variables["lat"][:], dtype="f8")
    lon = np.asarray(ds.variables["lon"][:], dtype="f8")
    iy = np.rint((LAT0 - lat) / RES).astype(int)
    ix = np.rint((lon - LON0) / RES).astype(int)
    if iy[0] > iy[-1]:
        raw, iy = raw[::-1], iy[::-1]
    if ix[0] > ix[-1]:
        raw, ix = raw[:, ::-1], ix[::-1]
    if not (np.array_equal(iy, np.arange(NLAT)) and np.array_equal(ix, np.arange(NLON))):
        raise ValueError("file is not on the global 0.05 degree CRW grid")
    return raw


def _pack(var, raw):
    """Return (flat packed integers, valid mask, multiplier, units)."""
    import numpy as np
    attrs = {k: var.getncattr(k) for k in var.ncattrs()}
    scale = float(attrs.get("scale_factor", 1.0))
    offset = float(attrs.get("add_offset", 0.0))
    fill = attrs.get("_FillValue")
    flat = raw.ravel()
    valid = np.ones(flat.shape, bool) if fill is None else flat != fill
    if np.issubdtype(flat.dtype, np.integer) and offset == 0.0:
        packed, mult = flat, int(round(1.0 / scale))
    else:
        valid &= np.isfinite(flat.astype("f8"))
        mult = 100
        packed = np.zeros(flat.shape, "i4")
        packed[valid] = np.rint((flat[valid].astype("f8") * scale + offset) * mult)
    return packed, valid, mult, str(attrs.get("units", ""))


def _smallest_int(a):
    import numpy as np
    if a.size == 0 or (a.min() >= -32768 and a.max() <= 32767):
        return a.astype("i2")
    return a.astype("i4")


def _data_vars(ds):
    return [v for n, v in ds.variables.items()
            if n not in SKIP_VARS and v.ndim >= 2 and v.dtype.kind in "iuf"]


def nc_to_table(nc_path, product, date):
    """Read one single-layer NOAA file -> (pyarrow Table, metadata dict)."""
    import netCDF4
    import numpy as np
    import pyarrow as pa
    with netCDF4.Dataset(nc_path) as ds:
        cands = _data_vars(ds)
        if not cands:
            raise ValueError(f"no data variable found in {nc_path}")
        var = cands[0]
        packed, valid, mult, units = _pack(var, _oriented(var, ds))
        varname = var.name
    cell_id = np.flatnonzero(valid).astype("i4")
    vals = _smallest_int(packed[valid])
    base = product.replace("-", "_")
    col = base if mult == 1 else f"{base}_x{mult}"
    table = pa.table({
        "date": pa.repeat(pa.scalar(date, pa.date32()), len(cell_id)),
        "cell_id": cell_id,
        col: vals,
    })
    meta = {
        "product": product, "value_column": col, "scale_factor": repr(1.0 / mult),
        "units": units, "source_variable": varname, "source_file": Path(nc_path).name,
        "grid": "global 0.05 deg; cell_id = iy*7200 + ix; "
                "lon = -179.975 + 0.05*ix; lat = 89.975 - 0.05*iy",
    }
    return table, meta


def climatology_to_table(nc_path):
    """All 2-D climatology variables -> one wide table keyed by cell_id."""
    import netCDF4
    import numpy as np
    import pyarrow as pa
    cols, valid_any = {}, None
    with netCDF4.Dataset(nc_path) as ds:
        for var in _data_vars(ds):
            packed, valid, mult, _ = _pack(var, _oriented(var, ds))
            name = var.name if mult == 1 else f"{var.name}_x{mult}"
            cols[name] = (packed, valid)
            valid_any = valid if valid_any is None else (valid_any | valid)
    cell_id = np.flatnonzero(valid_any).astype("i4")
    data = {"cell_id": cell_id}
    for name, (packed, valid) in cols.items():
        vals = _smallest_int(packed[valid_any])
        data[name] = pa.array(vals, mask=~valid[valid_any])
    meta = {"product": "climatology", "scale_factor": "0.01",
            "source_file": Path(nc_path).name}
    return pa.table(data), meta


def write_parquet(table, out_path, meta):
    """Atomic write: zstd + delta-encoded integers (dictionary for 'date')."""
    import pyarrow.parquet as pq
    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    tmp = out_path.with_name(out_path.name + ".tmp")
    single_date = "date" in table.column_names and meta.get("layout") != "region"
    delta = [c for c in table.column_names if not (single_date and c == "date")]
    table = table.replace_schema_metadata({k: str(v) for k, v in meta.items()})
    pq.write_table(
        table, tmp, compression="zstd", compression_level=ZSTD_LEVEL,
        use_dictionary=["date"] if single_date else False,
        column_encoding={c: "DELTA_BINARY_PACKED" for c in delta},
        row_group_size=ROW_GROUP, write_statistics=True,
    )
    os.replace(tmp, out_path)
    return out_path


def md5_of_file(path):
    h = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def convert_file(nc_path, root, md5=None, md5_verified=False):
    """Convert one NOAA file into the archive; return a manifest record."""
    parsed = parse_name(Path(nc_path).name)
    if parsed is None:
        raise ValueError(f"unrecognised NOAA filename: {Path(nc_path).name}")
    product, tf, date = parsed
    table, meta = nc_to_table(nc_path, product, date)
    md5 = md5 or md5_of_file(nc_path)
    meta.update(source_md5=md5, md5_verified=md5_verified, timeframe=tf)
    out = write_parquet(table, parquet_path(root, product, tf, date), meta)
    return {
        "product": product, "timeframe": tf, "date": date,
        "source_file": Path(nc_path).name, "source_md5": md5,
        "md5_verified": bool(md5_verified), "n_cells": table.num_rows,
        "parquet_path": str(out.relative_to(root)), "bytes": out.stat().st_size,
        "ingested_at": dt.datetime.now(dt.timezone.utc).replace(microsecond=0),
    }


# --------------------------------------------------------------------------
# Manifest
# --------------------------------------------------------------------------

def manifest_file(root):
    return Path(root) / "manifest" / "manifest.parquet"


def update_manifest(root, records):
    """Merge new records into <root>/manifest/manifest.parquet (atomic)."""
    import pyarrow as pa
    import pyarrow.compute as pc
    import pyarrow.parquet as pq
    if not records:
        return
    new = pa.Table.from_pylist(records)
    path = manifest_file(root)
    if path.exists():
        old = pq.read_table(path)
        keep = pc.invert(pc.is_in(old["parquet_path"], value_set=new["parquet_path"].combine_chunks()))
        new = pa.concat_tables([old.filter(keep), new.cast(old.schema)])
    new = new.sort_by([("timeframe", "ascending"), ("product", "ascending"), ("date", "ascending")])
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + ".tmp")
    pq.write_table(new, tmp, compression="zstd")
    os.replace(tmp, path)


# --------------------------------------------------------------------------
# NOAA server (FTP)
# --------------------------------------------------------------------------

def ftp_open(host, directory=None):
    ftp = FTP(host, timeout=120)
    ftp.login()
    if directory:
        ftp.cwd(directory)
    return ftp


def ftp_list(host, directory, retries=3):
    """Bare filenames in a remote directory; [] when it does not exist."""
    for attempt in range(1, retries + 1):
        try:
            ftp = ftp_open(host, directory)
            names = [Path(e).name for e in ftp.nlst()]
            ftp.quit()
            return names
        except error_perm:
            return []
        except Exception:
            if attempt == retries:
                raise
            time.sleep(2 ** attempt)
    return []


def remote_dirs(base, product, tf, year):
    """Candidate remote directories, most likely first."""
    stem = product.split("-")[0] if tf != "daily" else product
    nc = f"{base}/nc/v1.0"
    if tf == "annual":
        return [f"{nc}/annual"]
    if tf == "monthly":
        return [f"{nc}/monthly/{year}", f"{nc}/monthly/{stem}/{year}"]
    return [f"{nc}/daily/{product}/{year}"]


def list_remote(cfg, product, tf, years):
    """[(remote_dir, filename, product, date)] for the requested files."""
    host, base = cfg["server"]["host"], cfg["server"]["base"]
    out, annual_cache = [], None
    for year in years:
        for d in remote_dirs(base, product, tf, year):
            if tf == "annual":
                if annual_cache is None:
                    annual_cache = ftp_list(host, d)
                names = annual_cache
            else:
                names = ftp_list(host, d)
            hits = []
            for n in names:
                p = parse_name(n)
                if p and p[1] == tf and p[2].year == year and product_matches(p[0], product, tf):
                    hits.append((d, n, p[0], p[2]))
            if hits:
                out += hits
                break
    return sorted(set(out), key=lambda r: (r[2], r[3]))


def fetch_md5(ftp, filename):
    """Expected md5 from the NOAA .md5 sidecar, or None when absent."""
    buf = io.BytesIO()
    try:
        ftp.retrbinary(f"RETR {filename}.md5", buf.write)
    except error_perm:
        return None
    text = buf.getvalue().decode().strip()
    m = re.search(r"\b[0-9a-fA-F]{32}\b", text)
    return m.group(0).lower() if m else None


def fetch_and_convert(job):
    """Worker: download one file, verify md5, convert, tidy up."""
    host, rdir, name, root, keep_raw, verify, retries = job
    root = Path(root)
    product, tf, date = parse_name(name)
    raw_dir = root / "raw" / tf / product
    raw_dir.mkdir(parents=True, exist_ok=True)
    nc, part = raw_dir / name, raw_dir / (name + ".part")
    err = None
    for attempt in range(1, retries + 1):
        try:
            ftp = ftp_open(host, rdir)
            expected = fetch_md5(ftp, name) if verify else None
            if not (nc.exists() and expected and md5_of_file(nc) == expected):
                with open(part, "wb") as f:
                    ftp.retrbinary(f"RETR {name}", f.write)
                actual = md5_of_file(part)
                if expected and actual != expected:
                    raise IOError(f"md5 mismatch ({actual[:8]} != {expected[:8]})")
                os.replace(part, nc)
            else:
                actual = expected
            ftp.quit()
            rec = convert_file(nc, root, md5=actual, md5_verified=expected is not None)
            if not keep_raw:
                nc.unlink()
            return "ok", name, rec
        except error_perm as e:
            return "error", name, f"permanent FTP error: {e}"
        except Exception as e:
            err = e
            if part.exists():
                part.unlink()
            time.sleep(min(60, 2 ** attempt))
    return "fail", name, str(err)


# --------------------------------------------------------------------------
# Commands
# --------------------------------------------------------------------------

def cmd_check(args, cfg):
    print(f"Python {sys.version.split()[0]}  ({sys.executable})")
    ok = check_packages(verbose=True)
    if not ok:
        print(f"  Fix: {sys.executable} -m pip install -r {SCRIPT_DIR / 'requirements.txt'}")
    root = resolve_root(cfg, args.root)
    writable = os.access(root, os.W_OK)
    print(f"  [{'ok' if writable else 'READ-ONLY'}] data root: {root}")
    ok &= writable
    if args.network:
        try:
            d = f"{cfg['server']['base']}/nc/v1.0/daily/sst/{dt.date.today().year}"
            n = len(ftp_list(cfg["server"]["host"], d))
            print(f"  [ok] server reachable: {n} entries in {d}")
            ok &= n > 0
        except Exception as e:
            print(f"  [FAIL] server not reachable: {e}")
            ok = False
    print("All checks passed." if ok else "Checks failed.")
    return 0 if ok else 1


def cmd_update(args, cfg):
    require_packages()
    root = resolve_root(cfg, args.root)
    years = parse_years(args.years)
    host = cfg["server"]["host"]
    status = 0
    for tf in args.timeframe:
        if tf == "climatology":
            status |= update_climatology(root, cfg, args)
            continue
        for product in args.product:
            print(f"\n== {product} / {tf} / {years[0]}-{years[-1]} ==")
            remote = list_remote(cfg, product, tf, years)
            todo = [r for r in remote
                    if args.force or not parquet_path(root, r[2], tf, r[3]).exists()]
            print(f"  {len(remote)} on server, {len(remote) - len(todo)} already archived, "
                  f"{len(todo)} to fetch")
            if args.dry_run or not todo:
                continue
            jobs = [(host, d, n, str(root), args.keep_raw, not args.no_checksum, args.retries)
                    for d, n, _, _ in todo]
            records, bad = [], 0
            with ProcessPoolExecutor(max_workers=args.workers) as ex:
                futs = [ex.submit(fetch_and_convert, j) for j in jobs]
                for i, fut in enumerate(as_completed(futs), 1):
                    state, name, info = fut.result()
                    if state == "ok":
                        records.append(info)
                    else:
                        bad += 1
                        print(f"  {state.upper():5} {name}: {info}")
                    if len(records) >= 200:          # checkpoint the manifest
                        update_manifest(root, records)
                        records = []
                    if i % 50 == 0 or i == len(futs):
                        print(f"  {i}/{len(futs)} done", flush=True)
            update_manifest(root, records)
            print(f"  finished: {len(jobs) - bad} archived, {bad} failed")
            status |= int(bad > 0)
    return status


def update_climatology(root, cfg, args):
    host, base = cfg["server"]["host"], cfg["server"]["base"]
    out = Path(root) / "climatology" / "climatology.parquet"
    if out.exists() and not args.force:
        print("\n== climatology ==\n  already archived")
        return 0
    for d in (f"{base}/climatology/nc", f"{base}/climatology"):
        names = [n for n in ftp_list(host, d) if n.endswith(".nc")]
        if names:
            break
    else:
        print("  climatology file not found on server")
        return 1
    raw = Path(root) / "raw" / "climatology"
    raw.mkdir(parents=True, exist_ok=True)
    nc = raw / names[0]
    ftp = ftp_open(host, d)
    with open(nc, "wb") as f:
        ftp.retrbinary(f"RETR {names[0]}", f.write)
    ftp.quit()
    table, meta = climatology_to_table(nc)
    write_parquet(table, out, meta)
    if not args.keep_raw:
        nc.unlink()
    print(f"\n== climatology ==\n  wrote {out} ({table.num_rows:,} cells)")
    return 0


def cmd_ingest(args, cfg):
    require_packages()
    root = resolve_root(cfg, args.root)
    files = []
    for p in args.paths:
        p = Path(p)
        files += sorted(p.glob("*.nc")) if p.is_dir() else [p]
    records = []
    for f in files:
        parsed = parse_name(f.name)
        if parsed is None:
            print(f"  SKIP  {f.name} (unrecognised name)")
            continue
        if parquet_path(root, *parsed).exists() and not args.force:
            print(f"  SKIP  {f.name} (already archived)")
            continue
        sidecar = Path(str(f) + ".md5")
        actual, verified = md5_of_file(f), False
        if sidecar.exists():
            m = re.search(r"\b[0-9a-fA-F]{32}\b", sidecar.read_text())
            if m and m.group(0).lower() != actual:
                print(f"  FAIL  {f.name} (md5 mismatch with sidecar)")
                continue
            verified = bool(m)
        rec = convert_file(f, root, md5=actual, md5_verified=verified)
        records.append(rec)
        print(f"  OK    {f.name} -> {rec['parquet_path']} "
              f"({rec['bytes'] / 1e6:.1f} MB, {f.stat().st_size / 1e6:.1f} MB source)")
        if args.delete_source:
            f.unlink()
    update_manifest(root, records)
    return 0


def region_bounds(cfg, args):
    if args.bbox:
        lon0, lon1, lat0, lat1 = args.bbox
    else:
        r = cfg["regions"][args.region]
        (lon0, lon1), (lat0, lat1) = r["lon"], r["lat"]
    import math
    ix0 = max(0, math.ceil((lon0 - LON0) / RES - 1e-6))
    ix1 = min(NLON - 1, math.floor((lon1 - LON0) / RES + 1e-6))
    iy0 = max(0, math.ceil((LAT0 - lat1) / RES - 1e-6))
    iy1 = min(NLAT - 1, math.floor((LAT0 - lat0) / RES + 1e-6))
    return ix0, ix1, iy0, iy1


def cmd_extract(args, cfg):
    """Regional subset, one file per year, sorted by (cell_id, date)."""
    require_packages()
    import numpy as np
    import pyarrow as pa
    import pyarrow.parquet as pq
    root = resolve_root(cfg, args.root)
    ix0, ix1, iy0, iy1 = region_bounds(cfg, args)
    lo, hi = iy0 * NLON + ix0, iy1 * NLON + ix1
    years = set(parse_years(args.years))
    reg_root = root / "regions" / args.region
    for tf in args.timeframe:
        tf_dir = root / tf
        if not tf_dir.is_dir():
            continue
        products = [d.name for d in sorted(tf_dir.iterdir()) if d.is_dir()
                    and any(product_matches(d.name, w, tf) for w in args.product)]
        for product in products:
            groups = {}
            for f in sorted((tf_dir / product).rglob("*.parquet")):
                date = parse_name(f"ct5km_{product}_v3.1_{f.stem.split('_')[-1]}.nc")[2]
                if date.year in years:
                    groups.setdefault("all" if tf == "annual" else date.year, []).append(f)
            for key, files in sorted(groups.items(), key=lambda kv: str(kv[0])):
                out = reg_root / tf / product / f"{product}_{args.region}_{key}.parquet"
                sig = hashlib.md5("|".join(
                    f"{f.name}:{f.stat().st_size}" for f in files).encode()).hexdigest()
                if out.exists() and not args.force:
                    old = pq.read_schema(out).metadata or {}
                    if old.get(b"source_signature", b"").decode() == sig:
                        continue
                parts = []
                for f in files:
                    t = pq.read_table(f, partitioning=None,
                                      filters=[("cell_id", ">=", lo), ("cell_id", "<=", hi)])
                    ix = t["cell_id"].to_numpy() % NLON
                    parts.append(t.filter(pa.array((ix >= ix0) & (ix <= ix1))))
                meta = {k.decode(): v.decode() for k, v in
                        (pq.read_schema(files[0]).metadata or {}).items()
                        if k.decode() in ("product", "value_column", "scale_factor",
                                          "units", "grid", "timeframe")}
                meta.update(layout="region", region=args.region, source_signature=sig,
                            n_source_files=len(files),
                            bbox_cells=f"ix {ix0}-{ix1}, iy {iy0}-{iy1}")
                table = pa.concat_tables(parts).sort_by(
                    [("cell_id", "ascending"), ("date", "ascending")])
                write_parquet(table, out, meta)
                print(f"  {out.relative_to(root)}: {table.num_rows:,} rows from "
                      f"{len(files)} file(s), {out.stat().st_size / 1e6:.2f} MB")
    # cell lookup for the region (cell_id, lon, lat) - ocean/land not distinguished
    iy, ix = np.meshgrid(np.arange(iy0, iy1 + 1), np.arange(ix0, ix1 + 1), indexing="ij")
    cells = pa.table({"cell_id": (iy * NLON + ix).ravel().astype("i4"),
                      "lon": np.round(LON0 + RES * ix.ravel(), 3),
                      "lat": np.round(LAT0 - RES * iy.ravel(), 3)})
    reg_root.mkdir(parents=True, exist_ok=True)
    pq.write_table(cells, reg_root / "cells.parquet", compression="zstd")
    return 0


def cmd_status(args, cfg):
    root = resolve_root(cfg, args.root)
    print(f"Data root: {root}")
    for scope, base in (("archive", root), ("regions", root / "regions")):
        tops = [base] if scope == "archive" else sorted(p for p in base.glob("*") if p.is_dir())
        for top in tops:
            for tf in TIMEFRAMES:
                if not (top / tf).is_dir():
                    continue
                for prod in sorted(p for p in (top / tf).iterdir() if p.is_dir()):
                    files = sorted(prod.rglob("*.parquet"))
                    if not files:
                        continue
                    size = sum(f.stat().st_size for f in files) / 1e9
                    label = f"{tf}/{prod.name}" if scope == "archive" \
                        else f"regions/{top.name}/{tf}/{prod.name}"
                    print(f"  {label:34} {len(files):6d} files  {size:8.2f} GB  "
                          f"{files[0].stem.split('_')[-1]} .. {files[-1].stem.split('_')[-1]}")
    raw = sum(1 for _ in (root / "raw").rglob("*.nc")) if (root / "raw").is_dir() else 0
    print(f"  raw NetCDF files retained: {raw}")
    return 0


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------

def build_parser():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--root", help="data root (overrides CRW_ROOT and the config file)")
    p.add_argument("--config", help="path to crw_config.json")
    sub = p.add_subparsers(dest="cmd", required=True)

    def common(sp, timeframes):
        sp.add_argument("--product", nargs="+", default=["sst"],
                        help="e.g. sst dhw; monthly/annual 'sst' covers sst-mean/min/max")
        sp.add_argument("--timeframe", nargs="+", default=["daily"], choices=timeframes)
        sp.add_argument("--years", nargs="+", help="e.g. 1985-2026 or 2024 2025 (default: all)")
        sp.add_argument("--force", action="store_true", help="rebuild existing outputs")

    s = sub.add_parser("check", help="verify packages, data root, server")
    s.add_argument("--network", action="store_true", help="also test the NOAA server")
    s = sub.add_parser("update", help="download and archive new files")
    common(s, TIMEFRAMES + ("climatology",))
    s.add_argument("--workers", type=int, default=4)
    s.add_argument("--retries", type=int, default=5)
    s.add_argument("--keep-raw", action="store_true", help="keep NetCDF files under raw/")
    s.add_argument("--no-checksum", action="store_true")
    s.add_argument("--dry-run", action="store_true", help="list what would be fetched")
    s = sub.add_parser("ingest", help="convert local NetCDF files")
    s.add_argument("paths", nargs="+", help="NetCDF files or directories")
    s.add_argument("--force", action="store_true")
    s.add_argument("--delete-source", action="store_true")
    s = sub.add_parser("extract", help="build a regional subset")
    common(s, TIMEFRAMES)
    s.add_argument("--region", default="gbr", help="region name (config) or label for --bbox")
    s.add_argument("--bbox", nargs=4, type=float, metavar=("LON0", "LON1", "LAT0", "LAT1"))
    sub.add_parser("status", help="summarise the archive")
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)
    cfg = load_config(args.config)
    return {"check": cmd_check, "update": cmd_update, "ingest": cmd_ingest,
            "extract": cmd_extract, "status": cmd_status}[args.cmd](args, cfg)


if __name__ == "__main__":
    sys.exit(main())
