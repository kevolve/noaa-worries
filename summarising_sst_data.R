#### Install dependencies ####

# Install and load the required packages (if not installed previously):
if (!require("tidyverse")) {install.packages("tidyverse"); require(tidyverse)}
if (!require("purrr")) {install.packages("purrr"); require(purrr)}
if (!require("sf")) {install.packages("sf"); require(sf)}
if (!require("lubridate")) {install.packages("lubridate"); require(lubridate)}
if (!require("data.table")) {install.packages("data.table"); require(data.table)}

# Load the file:
load(file = "data/sst/sst_daily_wide_across_gbr_reefs.RData")
gbr_reefs <- full_join(read.csv("data/gbr_reef_coords/gbr_reefs.csv"), 
			   read.csv("data/climatology/gbr_climatology.csv"), by = join_by(reef_index))
sst_daily_wide <- sst_daily_wide %>% 
	mutate(date = as.Date(date),
		   year = lubridate::year(date)) %>%
	arrange(date)
reef_index <- unique(sst_daily_wide$reef_index)
years <- colnames(sst_daily_wide)[-1] %>% as.Date() %>%
	lubridate::year()
unique_years <- unique(years)

# Summarise yearly mean SST:
out_i <- vector(mode = "list", length = length(unique_years))
for (i in seq_along(unique_years)) {
	out_i[[i]] <- sst_daily_wide[,-1][, years == unique_years[i]] %>%
		apply(1, mean) %>%
		tibble(reef_index, year = unique_years[i], sst = .)
}
sst_yearly <- data.table::rbindlist(out_i)
save(sst_yearly, file = "data/sst/sst_yearly_long_across_gbr_reefs.RData")

# Save as wide format:
sst_yearly_wide <- sst_yearly %>%
	pivot_wider(names_from = year, values_from = sst)
save(sst_yearly_wide, file = "data/sst/sst_yearly_wide_across_gbr_reefs.RData")
write.csv(sst_yearly_wide, file = "data/sst/sst_yearly_wide_across_gbr_reefs.csv",
		  row.names = FALSE)

#### Plotting yearly data ####
sst_yearly %>%
	group_by(reef_index) %>%
	mutate(mean_sst = mean(sst)) %>%
	ggplot(aes(x = year, y = sst)) +
	geom_line(aes(color = mean_sst, group = reef_index),
			  show.legend = FALSE, alpha = 0.1, linewidth = 0.1) +
	geom_smooth(color = "black", se = FALSE) +
	scale_color_gradientn(colours = c('#303890', '#5065aa', '#7094c3', '#95c3d7', '#d5ede1', '#ffc3b1', '#f68683', '#df4759', '#b30034')) +
	labs(x = "Year", y = "Mean annual SST (°C)",
		 title = "Temperature variation across all GBR reefs")
ggsave(filename = "plots/yearly_temp_all_reefs.pdf", width = 5, height = 4)


#### Moore Reef specific analysis ####

# Determine index of Moore Reef
moore_index <- gbr_reefs %>% 
	filter(gbr_name == "Moore Reef") %>% 
	pull(reef_index)
# hinch_index <- gbr_reefs %>% 
# 	filter(gbr_name == "Hinchinbrook Reef") %>% 
# 	pull(reef_index)

# Combine and pre-process Moore Reef data for plotting:
moore_data <- filter(sst_daily, reef_index == moore_index) %>%
	left_join(gbr_reefs) %>%
	mutate(date = as.Date(date)) %>%
	arrange(date) %>% 
	mutate(year = year(date),
	   no_year_date = as.Date(paste0("2000-",month(date), "-", day(date))),
	   # date_plus7m = date %m+% months(7),
	   # annum = lubridate::year(date_plus7m), 
	   index = 1:n(),
	   .after = date) %>%
	mutate(sst_6w_mean = data.table::frollmean(sst, n=7*6, align = "center")) %>%
	group_by(year) %>%
	mutate(mean_sst = mean(sst),1,
		   mean_sst_txt = format(round(mean_sst, 1), nsmall = 1)) %>% ungroup()


#### Plotting Moore Reef SST data ####

# Create simple plot of all years
(p1 <- moore_data %>% 
	ggplot(aes(x = no_year_date, y = sst_6w_mean)) +
	geom_line(aes(color = mean_sst, group = year)) +
	scale_x_date(labels = scales::date_format("%b"), date_breaks = "month",
				 expand = expansion(add=c(0,0))) +
	scale_color_gradientn(colours = c('#303890', '#5065aa', '#7094c3', '#95c3d7', '#e5edd1', '#ffc3b1', '#f68683', '#df4759', '#b30034')) +
	labs(x = NULL, y = "Sea surface temperature\n(rolling 6-week mean °C)",
		 color = "Mean annual\nSST (°C)",
		 title = "Moore Reef (Cairns, Australia)") )

# Create an animated version of the plot
require(gganimate)
animate(p1 + transition_reveal(index) +
			guides(color = "none") +
			theme(plot.subtitle=element_text(hjust=0.5)) +
			labs(subtitle = "Year {moore_data$year[which(moore_data$index == frame_along)]} - Mean SST: {moore_data$mean_sst_txt[which(moore_data$index == frame_along)]}°C"), 
		width = 4.5, height = 3, units =  "in",
		res = 300,
		nframes=300,
		duration = 7,
		renderer = gifski_renderer(),
		end_pause = 10,
		bg="white")
anim_save("plots/moore_reef_temp_animated.gif")



#### Calculating DHWs ####

# Mutate dHWs
dhw_data_daily_tmp2 <- dhw_data_daily_tmp  %>%
	# need SST, MMM, site, and date to calculate it all!
	# filter(site == "Hinchinbrook Reef") %>%
	# requires sst, mmm, 
	# Calculate DHWs for 12-weeks after start for each site
	# first 84 values (12 weeks) of data need to be filled in with heat stress, but then
	# after first 12 weeks, use previous 12 weeks to fill in average °C-weeks
	# sst anomaly is difference from observed SST and monthly mean (MM) 
	group_by(site) %>%
	arrange(site, date) %>%
	mutate(sst_anomaly = ifelse(sst-mmm > 0, sst-mmm, 0), # calculate SST anomaly
		   heat_stress = ifelse(sst_anomaly >=0, sst_anomaly, 0),
		   heat_stress_1 = ifelse(heat_stress > 1, heat_stress, 0))

dhw_data_daily_tmp3 <- dhw_data_daily_tmp2 %>%
	group_by(site) %>%
	mutate(dhw = data.table::frollsum(heat_stress_1, n=12*7, align = "right", fill = 0)/7,
		   sst_12wk_rolling_mean = data.table::frollmean(sst, n=12*7, align = "center", fill = 0))
# frollsum is significantly faster than other options!!

View(filter(dhw_data_daily_tmp3, date > as.Date("1986-12-01")))

		   # min_sst = min(sst)) %>%
	ungroup() %>%
	left_join(y, by = "site")
save(dhw_data_daily, file = "data/Heron_sst_dhws/dhw_data_daily.RData")



dhw_data <- dhw_data_daily %>%
	rename(reef = site) %>%
	mutate(year = year(date)) %>%
	group_by(reef, year, sector, lon, lat) %>%
	summarise(dhw = max(dhw), .groups = "drop")


#### Summarising the data (computationally expensive!!) ####

# Summarising DHWs:
gbr_reefs2 <- gbr_data %>%
	dplyr::select(ref_ndx, gbr_nm_x, ams_sct) %>%
	st_drop_geometry() %>%
	rename(reef_index = ref_ndx, reef_name = gbr_nm_x, aims_sector = ams_sct) %>%
	full_join(sst_daily, by = join_by(reef_index))

# gbr_reefs3 <- gbr_reefs2 %>% 
# 	# filter(reef_index %in% 1:20) %>% 
# 	group_by(reef_index, reef_name, aims_sector, date) %>%
# 	summarise(mean_sst = mean(sst)) # VERY SLOW!!

# Use data.table operations to summarise??
setDT(gbr_reefs2)
my_summary = function(x) list(mean = mean(x, na.rm = T),
							  sd = sd(x, na.rm = T))
gbr_reefs3 <- 
	gbr_reefs2[, unlist(lapply(.SD, my_summary), recursive = FALSE),
			   by = .(reef_index, reef_name, aims_sector, date),
			   .SDcols = c('sst')]

gbr_reefs3 %>%
	mutate(date = as.Date(date)) %>%
	ggplot(aes(x = date, y = mean_sst)) +
	geom_line(show.legend = TRUE, aes(color = factor(aims_sector)))

