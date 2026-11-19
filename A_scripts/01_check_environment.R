#### 01 - Check the environment ####
# Confirms the data root, the Python interpreter and the Python packages
# (numpy, netCDF4, pyarrow) needed to build the archive.
# Run from the repository root.

library(dplyr)
invisible(lapply(list.files("A_scripts/functions", pattern = "\\.R$", full.names = TRUE), source))

# Optional overrides (otherwise taken from A_scripts/functions/crw_config.json):
# options(crw.root = "/QRISdata/Q5947/noaa")
# options(crw.python = "/path/to/python3")

crw_root()
crw_python()
crw_check_python(network = TRUE)   # network = TRUE also tests the NOAA server
crw_status()
