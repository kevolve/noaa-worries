#### 04 - Read time series and calculate degree heating weeks ####
# Run from the repository root after scripts 02 and 03.

library(dplyr)
library(ggplot2)
invisible(lapply(list.files("A_scripts/functions", pattern = "\\.R$", full.names = TRUE), source))

#### Reef coordinates ####
reefs <- read.csv("C_gbr_data/gbr_reefs.csv")
moore <- reefs %>% filter(gbr_name == "Moore Reef") %>% slice(1)

#### Annual mean SST at one reef ####
annual <- crw_read_points(moore$lon, moore$lat, product = "sst-mean",
                          timeframe = "annual", region = "gbr")
annual %>%
  ggplot(aes(x = date, y = sst_mean)) +
  geom_line() +
  labs(x = NULL, y = "Annual mean SST (°C)", title = "Moore Reef")

#### Daily SST and degree heating weeks ####
# HotSpot = SST - MMM (negative values set to 0).
# DHW     = sum over the preceding 84 days of HotSpot / 7, counting only days
#           with HotSpot >= 1 °C (NOAA Coral Reef Watch v3.1 methodology).
daily <- crw_read_points(moore$lon, moore$lat, product = "sst",
                         timeframe = "daily", region = "gbr")
clim  <- crw_climatology(cell_id = moore$cell_id)
mmm   <- clim$sst_clim_mmm   # check names(clim) if this column is absent

daily <- daily %>%
  mutate(hotspot = crw_hotspot(sst, mmm),
         dhw     = crw_dhw(sst, mmm, date = date))

# Compare against NOAA's own DHW product:
noaa_dhw <- crw_read_points(moore$lon, moore$lat, product = "dhw",
                            timeframe = "daily", region = "gbr") %>%
  select(date, dhw_noaa = dhw)

daily %>%
  left_join(noaa_dhw, by = "date") %>%
  filter(date >= as.Date("2015-06-01")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = dhw_noaa), colour = "grey60", linewidth = 1.2) +
  geom_line(aes(y = dhw), colour = "firebrick") +
  labs(x = NULL, y = "DHW (°C-weeks)",
       title = "Recalculated (red) against NOAA (grey) degree heating weeks")

#### Working with the dataset directly ####
# crw_open() returns an Arrow dataset; filters run before data are loaded.
crw_open("sst-mean", "annual", region = "gbr") %>%
  filter(date >= as.Date("2010-01-01")) %>%
  collect() %>%
  crw_scale() %>%
  group_by(date) %>%
  summarise(mean_sst = mean(sst_mean), .groups = "drop")
