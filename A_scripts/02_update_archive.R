#### 02 - Update the archive ####
# Fetches files that are on the NOAA server but not yet in the archive.
# Safe to re-run: archived files are skipped and failed files are retried.
# Run from the repository root.

library(dplyr)
invisible(lapply(list.files("A_scripts/functions", pattern = "\\.R$", full.names = TRUE), source))

# Preview without downloading:
crw_update(product = "sst", timeframe = "daily", years = 2026, dry_run = TRUE)

# Climatology (maximum monthly mean and monthly means):
crw_update(timeframe = "climatology")

# Annual and monthly summaries, all years:
crw_update(product = c("sst", "dhw"), timeframe = c("annual", "monthly"))

# Daily SST and DHW, all years (large: use the HPC job in B_hpc/ for the
# first build, then this call for routine updates):
crw_update(product = c("sst", "dhw"), timeframe = "daily", workers = 4)

crw_status()
