require(tidyverse)
require(sf)
require(raster)

# Load a shape file with coordinates for use:
gbr_reefs <- read_sf("data/gbr_reef_coords/gbr_reefs.shp")
moore <- gbr_reefs %>% filter(gbr_nm_x == "Moore Reef") # try with a single reef for now

# Note that single long-lat coordinates for each reef are required! Not polygons!
# So use st_centroid() to convert to single coordinates if required!


base_file_path <- "/Users/uqkbairo/MODRRAP/storage1tb/data/raw/sst/"
years <- 2014

nc_files <- list.files(paste0(base_file_path, years[i], "/"))
file_path <- paste0(base_file_path, years[i], "/",nc_files[ii])
r <- raster::raster(file_path, varname = "analysed_sst")

extracted_sst <- 
	gbr_reefs %>%
	st_drop_geometry() %>% # drop polygons...
	st_as_sf(coords = c("lon","lat")) %>% # just use centroids...
	raster::extract(r, .)

(na_i <- which(is.na(extracted_sst))) # there are 5 locations producing NAs! 
# Will ignore for now...

# Overwrite without NA value locations (n=5) or can modify lat-lon to include grids with less land mass that would have SST!
gbr_reefs <- gbr_reefs[-na_i,]


#### Looping through all files ####

# Now loop through SST and save values through time, based on unique reef ID and date:
base_file_path <- "/Users/uqkbairo/MODRRAP/storage1tb/data/raw/sst/"
years <- 2003:2023
i <- 1
ii <- 1
target <- gbr_reefs



target_reefs <- target %>%
	st_drop_geometry() %>% # drop polygons...
	st_as_sf(coords = c("lon","lat")) # set specific coordinates to extract for...

# Could also try it with polygons, but missing 140 reefs...
# would then use:
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
	runtime1 = round(as.numeric(difftime(Sys.time(), year_tm, units = "mins")),2) # record run time
	runtime2 = round(as.numeric(difftime(Sys.time(), start_tm, units = "mins")),2) # record run time
	
	cat(paste0("Year ", years[[i]]," extractions complete in ", runtime1," mins! (total runtime: ", runtime2," mins)\n\n"))
	
}
out <- data.table::rbindlist(out_i)
round(as.numeric(difftime(Sys.time(), start_tm, units = "mins")),2) # final run time

# Save files:
write.csv(out, file = "data/daily_sst_across_gbr_reefs.csv", row.names = FALSE)
# Approx 20 mins per year with 16GB RAM, so:
20*length(1985:2023)/60 # ~13 hours to run to completion!



# Where it fails, delete incorrect files and re-run download:
download_netcdf_files(output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data/raw", 
					  years = 2003, type = "daily", measure = "sst")





r <- raster::raster(file_path, varname = "analysed_sst")

extracted_sst <- 
	gbr_reefs %>%
	st_drop_geometry() %>% # drop polygons...
	st_as_sf(coords = c("lon","lat")) %>% # just use centroids...
	raster::extract(r, .)

# (nc <- nc_open(file_path))
# var_names <- purrr::map(1:nc$nvars, function(x){nc$var[[x]]$name}) %>% unlist()
# print(paste("This file has",nc$nvars,"variables:", paste0(var_names, collapse = ", ")))



require(sf)


shape <- shape %>% 
	filter(level_2 == "Coral Reef") %>%
	filter(country == "Australia")

shape %>% 
	filter(lat == "-10.7966", lon = "142.7106")
shape$unique_id
names(shape)
View(shape)
ncvar_get(nc, "analysed_sst")

r <- raster(file_path, varname = "analysed_sst")

ncatt_get(nc, var_names[3], "units")
epsg_code <- ncatt_get(nc, var_names[3], "epsg_code")$value

raster()
file_names <- getURL(url, ftp.use.epsv = FALSE, crlf = TRUE, dirlistonly = TRUE)
file_names <- strsplit(file_names, "\r*\n") %>% unlist() %>%
	str_match("ct5km_dhw-max_v3.1.*\\.nc$") %>%
	na.omit() %>% as.vector()
file_names # list all file names to access across the FTP server
urls <- paste0(url, file_names)[c(1,36)] # use last value if already done once (36 is 2021)
nc_files  <- paste0("./netCDF/",file_names)[c(1,36)]


## Download the files:
tic()
map2(urls, nc_files, curl::curl_download)
toc() # takes 8+ minutes for full download
# Note: The CRS for 2020 and more recent years is different from 2019 previous

#### Convert and save all netCDF files as GeoTiffs ####

nc_to_geotiff <- function(file_path, crs_to_use="+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") {
	require(tictoc); tic()
	
	base_file <- basename(file_path)
	nc_open(file_path)
	r <- raster("netCDF/ct5km_dhw-max_v3.1_1986.nc", varname = "degree_heating_week")
	names(r) <- "dhw"
	crs(r) <- crs_to_use
	
	writeRaster(r,
				filename=paste0("./GeoTiff/",
								stringr::str_replace(base_file, ".nc$", ".tif")),
				format="GTiff",
				overwrite=TRUE)
	toc()
	paste("Successfully converted", base_file)
}


## Apply conversion function:
nc_files <- list.files("./netCDF/", pattern = ".nc$") %>%
	paste0("./netCDF/",.)
# raster(nc_files[1])

tic()
nc_files %>% # change to final value to speed up!
	purrr::map(~nc_to_geotiff(.x))
toc() # Takes a little over 10 minutes for all files if running slow


#### Re-save geotiff as a raster stack ####

gt_files <- list.files("./GeoTiff/", pattern = ".tif$") %>%
	paste0("./GeoTiff/",.)
gt1 <- raster(gt_files[1])
gt2 <- raster(gt_files[2])
crs(gt1, asText=TRUE)
crs(gt2, asText=TRUE)
## Create a raster stack of all GeoTiffs:
s <- gt_files %>%
	# as.list() %>%
	purrr::map(raster) %>%
	raster::stack()

## Save raster stack and close shop
s <- stackSave(s, "dhw_stack_globe.stk")
rm(list=ls()); removeTmpFiles(h=0)


## Open and set up stack for use:
s <- stackOpen("dhw_stack_globe.stk")
names(s) <- 
	paste0("DHW_",str_extract(names(s), "(?<=ct5km_dhw.max_v3.1_).*"))

## Plot each raster:
plot(s[[1:16]])
plot(s[[17:32]])
plot(s[[33:36]])
