require(tidyverse)
require(lubridate)
require(purrr)

# Load the file (if closed after finishing):
load("data/sst_data__daily_across_gbr_reefs.RData") # load sst_data object (~20 secs)
gbr_reefs <- read_sf("data/gbr_reef_coords/gbr_reefs.shp")

# Determine index of Moore Reef
moore_index <- gbr_reefs %>% 
	filter(gbr_nm_x == "Moore Reef") %>% 
	pull(ref_ndx)

# Combine and pre-process Moore Reef data for plotting:
moore_data <- filter(sst_data, reef_index == moore_index) %>%
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


#### Plotting the SST data ####

# Create simple plot of all years
(p1 <- moore_data %>% 
	ggplot(aes(x = no_year_date, y = sst_6w_mean)) +
	geom_line(aes(color = mean_sst, group = year)) +
	scale_x_date(labels = scales::date_format("%b"), date_breaks = "month",
				 expand = expansion(add=c(0,0))) +
	scale_color_gradientn(colours = c('#303890', '#516aad', '#729dc8', '#9ed2df', '#dbe3c5', '#fec2b1', '#f58684', '#df4759', '#b30034')) +
	labs(x = NULL, y = "Sea surface temperature\n(rolling 6-week mean °C)",
		 color = "Mean annual SST (°C)",
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



#### Summarising the data (computationally expensive!!) ####

# Summarising DHWs:
gbr_reefs2 <- gbr_data %>%
	dplyr::select(ref_ndx, gbr_nm_x, ams_sct) %>%
	st_drop_geometry() %>%
	rename(reef_index = ref_ndx, reef_name = gbr_nm_x, aims_sector = ams_sct) %>%
	full_join(sst_data, by = join_by(reef_index))

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

