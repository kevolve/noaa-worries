#### 03 - Build the Great Barrier Reef subset ####
# Writes <root>/regions/gbr/, one file per year sorted by cell then date.
# Only years whose source files changed are rebuilt.
# Run from the repository root.

library(dplyr)
invisible(lapply(list.files("A_scripts/functions", pattern = "\\.R$", full.names = TRUE), source))

crw_extract_region(region  = "gbr",
                   product = c("sst", "dhw"),
                   timeframe = c("daily", "monthly", "annual"))

# Another region can be defined in A_scripts/functions/crw_config.json or given
# directly as c(lon_min, lon_max, lat_min, lat_max):
# crw_extract_region(region = "ningaloo", bbox = c(112.5, 115, -24.5, -21))
