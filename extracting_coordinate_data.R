require(tidyverse)
require(sf)
require(raster)
require(data.table)

# Load a shape file with coordinates for use:
gbr_reefs <- read_sf("data/gbr_reef_coords/gbr_reefs.shp")
# moore <- gbr_reefs %>% filter(gbr_nm_x == "Moore Reef") # try with a single reef for now

# Note that single long-lat coordinates for each reef are required! Not polygons!
# So use st_centroid() to convert to single coordinates if required!


#### Looping through all files ####

# Now loop through SST and save values through time, based on unique reef ID and date:
base_file_path <- "/Users/uqkbairo/MODRRAP/storage1tb/data/raw/sst/"
years <- 1988
target <- gbr_reefs



target_reefs <- target %>%
	st_drop_geometry() %>% # drop polygons...
	st_as_sf(coords = c("lon","lat")) # set specific coordinates to extract for...

# Could also try it with polygons, but missing 140 reefs...
# would then use (in function below):
# out_ii[[ii]] <- raster::extract(r, target_reefs, method = "bilinear", exact = TRUE)

# Looping code:
out_i <- vector(mode = "list", length = length(years))
start_tm = Sys.time() # record start time
cat("Starting SST extraction...\n")
for(i in seq_along(years)){
	year_tm = Sys.time() # record start time
	cat(paste0("\nStarting SST extractions for year ", years[[i]],"...\n"))
	
	nc_files <- list.files(paste0(base_file_path, years[i], "/")) %>% sort()
	out_ii <- vector(mode = "list", length = length(nc_files))
	dates <- nc_files %>%
		str_match(., regex("^coraltemp_v3.1_(.*).nc")) %>% {.[,-1]} %>%
		str_match(., regex("(\\d{4})(\\d{2})(\\d{2})")) %>% {.[,-1]} %>%
		`colnames<-`(c("d1", "d2", "d3")) %>%
		as.tibble() %>%
		mutate(date = paste0(d1,"-",d2,"-",d3)) %>% 
		pull(date)
	
	for(ii in seq_along(nc_files)){
		file_path <- paste0(base_file_path, years[i], "/",nc_files[ii])
		r <- raster::raster(file_path, varname = "analysed_sst")
		out_ii[[ii]] <- raster::extract(r, target_reefs) %>%
			tibble(reef_index = target_reefs$ref_ndx, date = dates[ii], sst = .)
		if(ii %% 5 == 0 | ii == 1){cat(paste0("\nCompleted ", dates[[ii]], "... "))} else {cat(paste0(dates[[ii]],"... "))}
	}
	out_i[[i]] <- data.table::rbindlist(out_ii)
	runtime1 = round(as.numeric(difftime(Sys.time(), start_tm, units = "mins")),2) # record run time
	runtime2 = round(as.numeric(difftime(Sys.time(), year_tm, units = "mins")),2) # record run time
	
	cat(paste0("Year ", years[[i]]," extractions complete in ", runtime2," mins! (total runtime: ", runtime1," mins)\n\n"))
	
}
out <- data.table::rbindlist(out_i)

# Save files:
# write.csv(out, file = "data/daily_sst_across_gbr_reefs.csv", row.names = FALSE)
# Run time variable; 5-20 mins/yr dependent on system memory available!


# # Merge files when there's multiple large files:
# files_to_merge <- list.files("data") %>%
# 	str_subset(regex("daily_sst_across_gbr_reefs.*.csv"))
# 
# out_i <- vector(mode = "list", length = length(files_to_merge))
# for(i in seq_along(files_to_merge)) {
# 	out_i[[i]] <- read.csv(file = paste0("data/",files_to_merge[i]))
# }
# sst_data <- data.table::rbindlist(out_i) %>% arrange(date, reef_index)
# # write.csv(sst_data, file = "data/sst_data__daily_across_gbr_reefs.csv", row.names = FALSE)
# save(sst_data, file = "data/sst_data__daily_across_gbr_reefs.RData")


# # Where it fails, delete incorrect files and re-run download:
# download_netcdf_files(output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data/raw",
# 					  years = 2023, type = "daily", measure = "sst")


# Load the file (if closed after finishing):
load("data/sst_data__daily_across_gbr_reefs.RData") # load sst_data object (~20 secs)

gbr_reefs2 <- gbr_reefs %>%
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

# #### Extracting for only a single file ####
# base_file_path <- "/Users/uqkbairo/MODRRAP/storage1tb/data/raw/sst/"
# years <- 2014
# i <- 1; ii <- 1
# nc_files <- list.files(paste0(base_file_path, years[i], "/"))
# file_path <- paste0(base_file_path, years[i], "/",nc_files[ii])
# r <- raster::raster(file_path, varname = "analysed_sst")
# extracted_sst <- 
# 	gbr_reefs %>%
# 	st_drop_geometry() %>% # drop polygons...
# 	st_as_sf(coords = c("lon","lat")) %>% # just use centroids...
# 	raster::extract(r, .)
# (na_i <- which(is.na(extracted_sst))) # there are 5 locations producing NAs! 
# # Will ignore for now...
# 
# # Overwrite without NA value locations (n=5) or can modify lat-lon to include grids with less land mass that would have SST!
# gbr_reefs <- gbr_reefs[-na_i,]