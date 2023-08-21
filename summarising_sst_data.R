#### Install dependencies ####

# Install and load the required packages (if not installed previously):
if (!require("tidyverse")) {install.packages("tidyverse"); require(tidyverse)}
if (!require("purrr")) {install.packages("purrr"); require(purrr)}
if (!require("sf")) {install.packages("sf"); require(sf)}
if (!require("lubridate")) {install.packages("lubridate"); require(lubridate)}
if (!require("data.table")) {install.packages("data.table"); require(data.table)}


#### Summarising monthly mean SST data ####
load(file = "data/sst/sst_daily_wide_across_gbr_reefs.RData")
sst_daily_wide <- sst_daily_wide %>% 
	mutate(date = as.Date(date),
		   year = lubridate::year(date)) %>%
	arrange(date)
reef_index <- unique(sst_daily_wide$reef_index)
years <- colnames(sst_daily_wide)[-1] %>% as.Date() %>%
	lubridate::year()
unique_years <- unique(years)
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

#### Plotting monthly data across all reefs####

load(file = "data/sst/sst_monthly_across_gbr_reefs.RData")
sst_monthly <- sst_daily %>%
	mutate(date = as.Date(date),
		   month = lubridate::month(date),
		   year = lubridate::year(date)) %>%
	group_by(reef_index, month, year) %>%
	summarise(sst = mean(sst)) %>%
	ungroup()

sst_monthly %>%
	group_by(reef_index) %>%
	mutate(mean_sst = mean(sst)) %>%
	ggplot(aes(x = date, y = sst)) +
	geom_line(aes(color = mean_sst, group = reef_index),
			  show.legend = FALSE, alpha = 0.1, linewidth = 0.1) +
	geom_smooth(color = "black", se = FALSE) +
	scale_color_gradientn(colours = c('#303890', '#5065aa', '#7094c3', '#95c3d7', '#d5ede1', '#ffc3b1', '#f68683', '#df4759', '#b30034')) +
	labs(x = "Year", y = "SST (°C)",
		 title = "Daily temperature variation across all GBR reefs")
ggsave(filename = "plots/daily_temp_all_reefs.pdf", width = 5, height = 4)


#### Summarising yearly mean SST data ####
load(file = "data/sst/sst_daily_wide_across_gbr_reefs.RData")
sst_daily_wide <- sst_daily_wide %>% 
	mutate(date = as.Date(date),
		   year = lubridate::year(date)) %>%
	arrange(date)
reef_index <- unique(sst_daily_wide$reef_index)
years <- colnames(sst_daily_wide)[-1] %>% as.Date() %>%
	lubridate::year()
unique_years <- unique(years)
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
		 title = "Yearly temperature variation across all GBR reefs")
ggsave(filename = "plots/yearly_temp_all_reefs.pdf", width = 5, height = 4)


#### Moore Reef specific analysis ####

# Load reef identifier file:
gbr_reefs <- full_join(read.csv("data/gbr_reef_coords/gbr_reefs.csv"), 
					   read.csv("data/climatology/gbr_climatology.csv"), by = join_by(reef_index))

# Determine index of Moore Reef
moore_index <- gbr_reefs %>% 
	filter(gbr_name == "Moore Reef") %>% 
	pull(reef_index)
hinch_index <- gbr_reefs %>%
	filter(gbr_name == "Hinchinbrook Reef") %>%
	pull(reef_index)

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



#### Calculating DHWs from daily SST ####
load(file = "data/sst/sst_daily_wide_across_gbr_reefs.RData")
sst_daily_wide <- arrange(sst_daily_wide, reef_index)
mmm <- read.csv(file = "data/climatology/gbr_climatology.csv") %>%
	dplyr::select(reef_index, sst_clim_mmm) %>%
	arrange(reef_index)
# Merge

sst_anomaly <- apply(sst_daily_wide[,-1] - as.matrix(mmm$sst_clim_mmm), 
					 1, function(x)ifelse(x >= 0, x, 0))
heat_stress <- apply(sst_anomaly, 1, function(x)ifelse(x >= 1, x, 0))

dhw <- heat_stress %>% 
	split(1:nrow(.)) %>% 
	purrr::map(~data.table::frollsum(.x, n=12*7, align = "right", fill = 0)/7) %>%
	map(as.matrix) %>%
	list_rbind()

# Summarise by year
sst_daily_wide <- sst_daily_wide %>% 
	mutate(date = as.Date(date),
		   year = lubridate::year(date)) %>%
	arrange(date)
reef_index <- unique(sst_daily_wide$reef_index)
years <- colnames(sst_daily_wide)[-1] %>% as.Date() %>%
	lubridate::year()
unique_years <- unique(years)
out_i <- vector(mode = "list", length = length(unique_years))
for (i in seq_along(unique_years)) {
	out_i[[i]] <- sst_daily_wide[,-1][, years == unique_years[i]] %>%
		apply(1, mean) %>%
		tibble(reef_index, year = unique_years[i], sst = .)
}
sst_yearly <- data.table::rbindlist(out_i)
save(sst_yearly, file = "data/sst/sst_yearly_long_across_gbr_reefs.RData")




