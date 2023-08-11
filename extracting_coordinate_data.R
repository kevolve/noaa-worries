#### Extracting data from netCDF files ####
#### (c) Kevin Novak, University of Queensland
#### Updated 9 Aug 2023



#### Install dependencies ####

# Install and load the required packages (if not installed previously):
if (!require("tidyverse")) {install.packages("tidyverse"); require(tidyverse)}
if (!require("sf")) {install.packages("sf"); require(purrr)}
if (!require("raster")) {install.packages("raster"); require(ncdf4)}
if (!require("data.table")) {install.packages("data.table"); require(tidyverse)}



#### Set options ####

## Load a target file with point longitude and latitude coordinates to extract:
target <- read.csv("data/gbr_reef_coords/gbr_reefs.csv")
# 'target' data frame must include the following columns:
#   * target$lon   - longitude of point coordinate to extract data for
#   * target$lat   - latitude of point coordinate to extract data for
#   * target$index - optional; unique factor (or numeric) specific to each point coordinate
# If data are shapefile polygons, use st_centroid() to convert to single point coordinates if needed

# Designate path used previously to download SST data from NOAA:
my_download_path = "/Users/uqkbairo/MODRRAP/storage1tb"

# Years in which to extract data from (can re-do specific years if failed previously):
years = 1988:2023

# Measure downloaded previously:
measure = "climatology"



#### Extract all files ####

target <- target %>% st_as_sf(coords = c("lon","lat")) # set geometry of coordinates to extract
if(is.null(target$index)) target$index <- 1:nrow(target)

# Custom functions to get proper paths through './data/raw/':
check_path <- function(path) {
	if(!file.exists(path)) stop(paste0("The path: '",path,"' does not exist or is incorrect and cannot be set! Please correct path and try again."))
	if(str_sub(path,-1,-1) == "/") out_path = path else out_path = paste0(path, "/")
	return(out_path)}
check_path2 <- function(path) {
	path <- check_path(path)
	if(!str_detect(path, regex("data/raw/$"))) {
		if(!str_detect(path, regex("data/$"))) {path = paste0(path, "data/raw/")} else {path = paste0(path, "raw/")}
	}
	return(check_path(path))
}
base_file_path <- paste0(check_path2(my_download_path), measure, "/")

# Looping code:
out_i <- vector(mode = "list", length = length(years))
start_tm = Sys.time() # record start time
cat("Starting SST extraction...\n")
if(measure == "climatology"){
	nc_file <- list.files(base_file_path) %>% 
		str_subset(., regex(".*.nc$"))
	file_path <- paste0(base_file_path,nc_file)
	var_names <- names(nc_open(file_path)$var[1:13])
	out_ii <- vector(mode = "list", length = length(var_names))
	for(ii in seq_along(var_names)){
		r <- raster::raster(file_path, varname = var_names[ii])
		out_ii[[ii]] <- raster::extract(r, target) %>%
			tibble(reef_index = target$index, varname = var_names[ii], sst = .)
	}
	out <- data.table::rbindlist(out_ii)
	write.csv(out, file = paste0(my_download_path,"/data/gbr_climatology.csv"), row.names = FALSE)
} # Currently broken from below here for the file paths!!***

if(measure == "sst") {
	for(i in seq_along(years)){
		year_tm = Sys.time() # record start time
		cat(paste0("\nStarting SST extractions for year ", years[[i]],"...\n"))
		
		nc_files <- list.files(paste0(base_file_path, years[i], "/")) %>% sort()
		out_ii <- vector(mode = "list", length = length(nc_files))
		dates <- nc_files %>%
			str_match(., regex("^coraltemp_v3.1_(.*).nc$")) %>% {.[,-1]} %>%
			str_match(., regex("(\\d{4})(\\d{2})(\\d{2})")) %>% {.[,-1]} %>%
			`colnames<-`(c("d1", "d2", "d3")) %>%
			as_tibble() %>%
			mutate(date = paste0(d1,"-",d2,"-",d3)) %>% 
			pull(date)
		
		for(ii in seq_along(nc_files)){
			file_path <- paste0(base_file_path, years[i], "/",nc_files[ii])
			r <- raster::raster(file_path, varname = "analysed_sst")
			out_ii[[ii]] <- raster::extract(r, target) %>%
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
		write.csv(out, file = paste0("data/extracted_data/partial_files/daily_sst_across_gbr_reefs_",year_range[1],".csv"), row.names = FALSE) else
			write.csv(out, file = paste0("data/extracted_data/partial_files/daily_sst_across_gbr_reefs_",year_range[1],"-",year_range[2],".csv"), row.names = FALSE)
	# Run time variable; 5-20 mins/yr dependent on system memory available!
}



#### Troubleshooting ####

### Where it fails, delete incorrectly downloaded files and re-run download:
download_netcdf_files(output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data/raw",
					  years = 2023, type = "daily", measure = "sst")



#### If you end up with multiple stored copies and want to merge them, use the following script:
files_to_merge <- list.files("data/extracted_data/") %>%
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



