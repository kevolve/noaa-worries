
#### Install/load dependencies ####

pkgs <- c("tidyverse", "purrr", "ncdf4", "RCurl", "curl", "arrow", "fs", "terra")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p); require(p, character.only = TRUE)
  }
}))

# Source the function script:
source("~/Documents/R/noaa-worries/functions/download_noaa_nc.R")


# Set a save directory (if one already exists, use this):
save_directory <- "/Users/uqkbairo/MODRRAP/storage1tb/data/sst/raw/"

#### Download data manually for the first time ####

# -- SST daily for 1985 --
results <- download_noaa_nc(
  output_path = save_directory,
  years       = 2025,
  measure     = "sst",
  timeframe   = "daily"
)

# -- DHW annual maxima for 2020:2025 --
results <- download_noaa_nc(
  output_path  = "/Users/uqkbairo/MODRRAP/noaa-worries",
  years        = 2020:2025,
  measure      = "dhw",
  timeframe    = "annual",
  summary_type = "max"
)

# -- Climatology only --
results <- download_noaa_nc(
  output_path = "/Users/uqkbairo/MODRRAP/noaa-worries",
  measure     = "climatology"
)

# -- Multiple timeframes --
results <- download_noaa_nc(
  output_path = "/Users/uqkbairo/MODRRAP/noaa-worries",
  years       = 1985,
  measure     = "sst",
  timeframe   = c("daily", "monthly", "annual", "climatology")
)

# -- Inspect results --
results %>% dplyr::filter(status %in% c("download_fail", "checksum_fail"))
results %>% dplyr::count(status)

#### Audit downloaded parquet files ####


