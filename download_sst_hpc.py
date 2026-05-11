from ftplib import FTP
from pathlib import Path
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed

def parse_args():
    parser = argparse.ArgumentParser(description="Download SST data from NOAA FTP server.")
    parser.add_argument(
        '--years',
        nargs='+',
        type=int,
        required=True,
        help='One or more years to download (e.g. --years 2024 2025)'
    )
    parser.add_argument(
        '--workers',
        type=int,
        default=4,
        help='Number of parallel download workers (default: 4)'
    )
    return parser.parse_args()

def download_file(file, remote_year_dir, local_year_dir):
    """Opens its own FTP connection and downloads a single file."""
    local_path = local_year_dir / file
    if local_path.exists():
        return f"  Skipping {file} (already exists)"
    
    try:
        ftp = FTP("ftp.star.nesdis.noaa.gov")
        ftp.login()
        ftp.cwd(remote_year_dir)
        with open(local_path, 'wb') as f:
            ftp.retrbinary(f'RETR {file}', f.write)
        ftp.close()
        return f"  Downloaded {file}"
    except Exception as e:
        return f"  Failed to download {file}: {e}"

def download_sst(years, workers):
    base_remote_dir = '/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/sst/'
    download_dir = Path('/Users/uqkbairo/Downloads/years')
    download_dir.mkdir(exist_ok=True)

    for year in years:
        year_str = str(year)
        remote_year_dir = base_remote_dir + year_str

        # Use a single connection just to list files
        try:
            ftp = FTP("ftp.star.nesdis.noaa.gov")
            ftp.login()
            ftp.cwd(remote_year_dir)
            files = ftp.nlst()
            ftp.close()
        except Exception as e:
            print(f"Could not access remote directory {remote_year_dir}: {e}")
            continue

        local_year_dir = download_dir / year_str
        local_year_dir.mkdir(exist_ok=True)
        print(f"[{year}] Found {len(files)} files — downloading with {workers} workers")

        with ThreadPoolExecutor(max_workers=workers) as executor:
            futures = {
                executor.submit(download_file, file, remote_year_dir, local_year_dir): file
                for file in files
            }
            for future in as_completed(futures):
                print(future.result())

    print("Done.")

if __name__ == "__main__":
    args = parse_args()
    download_sst(args.years, args.workers)

# Usage:

# Default 4 cores
# python download_sst_hpc.py --years 2024 2025

# Crank it up — but be mindful of server limits
# python download_sst_hpc.py --years 2024 2025 --workers 8

# Download all past to present files:
# python download_sst_hpc.py --years {1985..2026} --workers 8