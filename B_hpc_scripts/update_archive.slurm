#!/bin/bash
#SBATCH --job-name=crw_archive
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=48:00:00
#SBATCH --output=crw_archive_%j.log
# Add the account and partition lines required by the cluster, e.g.
# #SBATCH --account=<account>
# #SBATCH --partition=<partition>
#
# Builds or updates the archive under /QRISdata/Q5947/sst and refreshes the
# GBR subset. The job needs outbound FTP access to ftp.star.nesdis.noaa.gov.
# Submit from the repository root:  sbatch B_hpc_scripts/update_archive.slurm

set -euo pipefail

# Load or activate a Python environment here (cluster-specific), e.g.
# module load python
# source ~/venvs/crw/bin/activate

export CRW_ROOT=/QRISdata/Q5947/sst
CRW="python3 A_scripts/functions/crw_archive.py"
YEARS="1985-$(date +%Y)"

# Stops with installation instructions when a package is missing.
$CRW check --network

$CRW update --timeframe climatology
$CRW update --product sst dhw --timeframe annual monthly --years "$YEARS" --workers "$SLURM_CPUS_PER_TASK"
$CRW update --product sst dhw --timeframe daily --years "$YEARS" --workers "$SLURM_CPUS_PER_TASK"

$CRW extract --region gbr --product sst dhw --timeframe daily monthly annual
$CRW status
