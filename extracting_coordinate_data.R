require(tidyverse)
require(sf)

# Load a shape file with coordinates for use:
gbr_reefs <- read_sf("data/gbr_reef_coords/gbr_reefs.shp")

# If the data have coordinates based on (multi)polygons, 
# You'll need to convert to centroids for use with this script (reefs fall within 5km area, usually)


length(unique(gbr_reef_polygons_2023$unique_id))
length(unique(gbr_reefs$unique_id))
nrow(gbr_reefs)
anti_join(gbr_reef_polygons_2023, gbr_reefs)

nrow(left_join(gbr_reef_polygons_2023, shape, by = join_by(gbr_name, unique_id)) %>% filter(!is.na(priority_lb)))
nrow(inner_join(gbr_reef_polygons_2023, shape, by = join_by(unique_id)))
nrow(inner_join(gbr_reef_polygons_2023, shape, by = join_by(gbr_name)))


crs(gbr_reefs)

# Check to see centroids match up!
# sf_use_s2(FALSE) # need to turn off spherical geometry for it to work
# gbr_reefs[c("lon_b", "lat_b")] <- sf::st_centroid(gbr_reefs) %>% st_coordinates() 
# gbr_reefs %>% mutate(lon_diff = lon-lon_b, lat_diff = lat-lat_b) %>% dplyr::select(ends_with("diff")) # all lines up!
# st_is_valid(gbr_reefs) %>% any()# looks good!
View(x)
x %>% filter(gbr_name == "Moore Reef")


base_file_path <- "/Users/uqkbairo/MODRRAP/storage1tb/data/raw/sst/"
years <- 2014
i <- 1
ii <- 1
nc_files <- list.files(paste0(base_file_path, years[i], "/"))
file_path <- paste0(base_file_path, years[i], "/",nc_files[ii])
(nc <- nc_open(file_path))
r <- raster::raster(file_path, varname = "analysed_sst")
# var_names <- purrr::map(1:nc$nvars, function(x){nc$var[[x]]$name}) %>% unlist()
# print(paste("This file has",nc$nvars,"variables:", paste0(var_names, collapse = ", ")))
nc_lon_max <- ncatt_get(nc, "lon")[["valid_max"]]
nc_lon_min <- ncatt_get(nc, "lon")[["valid_min"]]
nc_lat_max <- ncatt_get(nc, "lat")[["valid_max"]]
nc_lat_min <- ncatt_get(nc, "lat")[["valid_min"]]





r_lat <- seq()
ncatt_get(nc, "dim")
# Load as raster:
dim(r) # 3600 long


point_series <- extract(r_brick, SpatialPoints(cbind(point_lon,point_lat)))





names(gbr_polygons)
names(reef_ids)
str(gbr_polygons)


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
