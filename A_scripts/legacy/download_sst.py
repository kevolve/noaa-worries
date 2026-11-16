from ftplib import FTP
from pathlib import Path
import argparse

def parse_args():
    parser = argparse.ArgumentParser(description="Download SST data from NOAA FTP server.")
    parser.add_argument(
        '--years',
        nargs='+',
        type=int,
        required=True,
        help='One or more years to download (e.g. --years 2024 2025)'
    )
    return parser.parse_args()

def download_sst(years):
    ftp = FTP("ftp.star.nesdis.noaa.gov")
    ftp.login()

    base_remote_dir = '/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/sst/'
    download_dir = Path('/Users/uqkbairo/Downloads/years')
    download_dir.mkdir(exist_ok=True)

    for year in years:
        year_str = str(year)
        remote_year_dir = base_remote_dir + year_str

        try:
            ftp.cwd(remote_year_dir)
        except Exception as e:
            print(f"Could not access remote directory {remote_year_dir}: {e}")
            continue

        local_year_dir = download_dir / year_str
        local_year_dir.mkdir(exist_ok=True)

        files = ftp.nlst()
        print(f"[{year}] Found {len(files)} files")

        for file in files:
            local_path = local_year_dir / file
            if local_path.exists():
                print(f"  Skipping {file} (already exists)")
            else:
                print(f"  Downloading {file}...")
                try:
                    with open(local_path, 'wb') as f:
                        ftp.retrbinary(f'RETR {file}', f.write)
                except Exception as e:
                    print(f"  Failed to download {file}: {e}")

    ftp.close()
    print("Done.")

if __name__ == "__main__":
    args = parse_args()
    download_sst(args.years)

## Usage:
# Single year
# python download_sst.py --years 2025

# Multiple specific years
# python download_sst.py --years 2023 2024 2025

# A range of years (using shell brace expansion)
# python download_sst.py --years {2020..2025}