#### Downloading netCDF files from FTP server ####
#### (c) Kevin Novak, University of Queensland
#### Updated 9 Aug 2023


## Uncomment (remove the '#' sign) and run the following code on first run:

# # Install the required packages (if not installed previously):
# install.packages(c("tidyverse", "sf", "raster", "data.table"))

# # Create the required directories for saving (if they don't exist already):
# dir.create("data") # run first time only
# dir.create("data/raw") # run first time only

## Load the required packages
require(tidyverse)
require(sf)
require(raster)
require(data.table)

## Load a shape file with coordinates for use:
gbr_reefs <- read_sf("data/gbr_reef_coords/gbr_reefs.shp") %>%
	`colnames<-`(unlist(read.csv("data/gbr_reef_coords/gbr_reefs_names.csv"))) %>%
	st_drop_geometry() %>% # drop polygons...
	st_as_sf(coords = c("lon","lat")) # set coordinates to extract


# Note that single long-lat coordinates for each reef are required! Not polygons!
# So use st_centroid() to convert to single coordinates if required!


#### Extract all files ####

# Set a target file with a column specifying the specific reef index (reef_index)
# to link this data with the soon-to-be extracted data
target <- gbr_reefs # set target with coordinates specified as 'geometry' in sf
base_file_path <- "/Users/uqkbairo/MODRRAP/storage1tb/data/raw/sst/" # set path to SST data
years <- 2023  # set years

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
		as_tibble() %>%
		mutate(date = paste0(d1,"-",d2,"-",d3)) %>% 
		pull(date)
	
	for(ii in seq_along(nc_files)){
		file_path <- paste0(base_file_path, years[i], "/",nc_files[ii])
		r <- raster::raster(file_path, varname = "analysed_sst")
		out_ii[[ii]] <- raster::extract(r, target_reefs) %>%
			tibble(reef_index = target$index, date = dates[ii], sst = .)
		# Extracting with polygons rather than points (untested):
		# out_ii[[ii]] <- raster::extract(r, target, method = "bilinear", exact = TRUE) %>% ...
		if(ii %% 5 == 0 | ii == 1){cat(paste0("\nCompleted ", dates[[ii]], "... "))} else {cat(paste0(dates[[ii]],"... "))}
	}
	out_i[[i]] <- data.table::rbindlist(out_ii)
	runtime1 = round(as.numeric(difftime(Sys.time(), start_tm, units = "mins")),2) # record run time
	runtime2 = round(as.numeric(difftime(Sys.time(), year_tm, units = "mins")),2) # record run time
	
	cat(paste0("Year ", years[[i]]," extractions complete in ", runtime2," mins! (total runtime: ", runtime1," mins)\n\n"))
	
}
out <- data.table::rbindlist(out_i) %>%
	mutate(year = str_match(date, regex("(\\d{4})-\\d{2}-\\d{2}"))[,-1])
out
# Save files:
year_range <- out %>% mutate(year = str_match(date, regex("(\\d{4})-\\d{2}-\\d{2}"))[,-1]) %>%
	pull(year) %>% range()
if(year_range[1] == year_range[2]) 
	write.csv(out, file = paste0("data/daily_sst_across_gbr_reefs_",year_range[1],".csv"), row.names = FALSE) else
	write.csv(out, file = paste0("data/daily_sst_across_gbr_reefs_",year_range[1],"-",year_range[2],".csv"), row.names = FALSE)

# Run time variable; 5-20 mins/yr dependent on system memory available!



#### Troubleshooting ####

### Where it fails, delete incorrectly downloaded files and re-run download:
download_netcdf_files(output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data/raw",
					  years = 2023, type = "daily", measure = "sst")



#### If you end up with multiple stored copies and want to merge them, use the following script:
files_to_merge <- list.files("data") %>%
	str_subset(regex("daily_sst_across_gbr_reefs.*.csv"))

# Confirm that the following files should be merged:
print(files_to_merge)

# Combine together:
out_i <- vector(mode = "list", length = length(files_to_merge))
for(i in seq_along(files_to_merge)) {
	out_i[[i]] <- read.csv(file = paste0("data/",files_to_merge[i]))
}
sst_data <- data.table::rbindlist(out_i) %>% arrange(date, reef_index)
# write.csv(sst_data, file = "data/sst_data__daily_across_gbr_reefs.csv", row.names = FALSE)
save(sst_data, file = "data/sst_data__daily_across_gbr_reefs.RData")



