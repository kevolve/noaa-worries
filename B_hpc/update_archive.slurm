#!/bin/bash
#SBATCH --job-name=crw_archive
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=48:00:00
#SBATCH --output=crw_archive_%j.log
#SBATCH --partition=general
#SBATCH --account=a_senv_mbos

# Builds or updates the archive under /QRISdata/Q5947/noaa and refreshes the
# GBR subset. The job needs outbound FTP access to ftp.star.nesdis.noaa.gov.
# Submit from the repository root:  sbatch B_hpc/update_archive.slurm

set -euo pipefail

# Purges modules, loads miniforge/25.3.0-3 and r/4.4.0-gfbf-2023a, activates
# the "crw" conda environment (created from environment.yml on first use)
# and checks the required packages.
source A_scripts/environment/activate_env.sh || exit 1

export CRW_ROOT=/QRISdata/Q5947/noaa
CRW="python3 A_scripts/functions/crw_archive.py"
YEARS="1985-$(date +%Y)"

# Stops with installation instructions when a package is missing.
$CRW check --network

$CRW update --timeframe climatology
$CRW update --product sst dhw --timeframe annual monthly --years "$YEARS" --workers "$SLURM_CPUS_PER_TASK"
$CRW update --product sst dhw --timeframe daily --years "$YEARS" --workers "$SLURM_CPUS_PER_TASK"

$CRW extract --region gbr --product sst dhw --timeframe daily monthly annual
$CRW status
