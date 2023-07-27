#### Downloading netCDF files from FTP server ####
#### (c) Kevin Novak, University of Queensland
#### Current as of 26 July 2023


## Uncomment (remove the '#' sign) and run the following code on first run:

# # Install the required packages (if not installed previously):
# install.packages(c("tidyverse", "purrr", "RCurl", "ncdf4"))

# # Create the required directories for saving (if they don't exist already):
# dir.create("data") # run first time only
# dir.create("data/raw") # run first time only

require(tidyverse)
require(purrr)
require(RCurl) # for downloading from ftp server
library(ncdf4)
library(raster)



# Testing:
output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data/raw"
type = "daily"
years = 1988:2023 # all available years
measure = "sst"


download_netcdf_files <- function(
		output_path = "data/raw/",
		type = c("annual", "monthly", "daily"),
		years = 1985,
		measure = c("sst", "baa", "baa-max-7d", "dhw", "hs", "ssta", "sst-trend-7d", "year-to-date_2022", "year-to-date", "climatology")) {
	
	list_ftp_files <- function(url) {
		require(RCurl)
		cat("Retrieving files...")
		RCurl::getURL(url, ftp.use.epsv = FALSE, crlf = TRUE, dirlistonly = TRUE) %>% 
			str_split(pattern="\n", simplify = TRUE) %>% 
			as.character() %>% 
			keep(str_detect(., "\\S"))
	}
	
	if(str_sub(output_path,-1,-1) != "/") output_path <- paste0(output_path, "/") # add a final slash if it doesn't have one
	if(!file.exists(output_path)) stop(paste0("File path '",output_path,"' does not exist!\nCannot begin download!"))
	if(!file.exists(paste0(output_path, measure))) dir.create(paste0(output_path, measure))
	if(!(measure %in% c("sst", "baa", "baa-max-7d", "dhw", "hs", "ssta", "sst-trend-7d", "year-to-date_2022", "year-to-date", "climatology"))) 
		stop("Invalid measure! Try one of the following: baa, baa-max-7d, cliatology, dhw, hs, sst, ssta, sst-trend-7d, year-to-date_2022, year-to-date")
	
	base_path = "ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/"
	
	if(measure == "climatology") {
		end_path = "climatology/nc"
		url <- paste0(base_path, end_path,"/")
		ftp_files <- list_ftp_files(url)
		output_file_paths <- paste0(output_path, measure,"/", ftp_files)
		
		# Download the paths
		cat(paste0("\nDownloading ",length(ftp_files)," files..."))
		map2(paste0(url, ftp_files), output_file_paths, curl::curl_download) # for downloading all in one fell swoop without live updating
		cat("Complete!\n")
		
		
	} else if (type %in% c("annual", "monthly", "daily")) {
		end_path = paste0("nc/v1.0/", type, "/")
		
		
		# from: https://www.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/
		# baa = bleaching alert area; sst = sea surface temperature; ssta = SST anomaly
		# hs = hot spot; dhw = degree heating weeks
		
		
		
		url <- paste0(base_path, end_path, measure, "/")
		years_avail <- list_ftp_files(url)
		
		
		for(i in seq_along(years)) {
			if(!(years[i] %in% years_avail)) {
				cat(paste0("The year '",years[i],"' was not found in the database!\n"))
			} else {
				ftp_files <- list_ftp_files(paste0(url,years[i],"/")) %>% 
					str_subset(pattern = regex("nc$")) %>% # must end with .nc (ignore checksum files ending with .md5)
					sort()
				closeAllConnections()
				cat("\n")
				
				if(!file.exists(paste0(output_path,measure))) stop("File path '",paste0(output_path,measure),"' does not exist!\nCannot begin download!")
				
				output_file_dir <-  paste0(output_path,measure,"/",years[i],"/")
				if(!file.exists(output_file_dir)) dir.create(output_file_dir) # create the year's file if it doesn't exist already
				output_file_paths <- paste0(output_file_dir, ftp_files)
				
				# Download all files using curl's multi_download (in parallel; allows for resuming of large files)
				cat(paste0("Starting year ",years[i], " download... (",length(ftp_files)," files)"))
				start_tm = Sys.time() # record start time
				curl::multi_download(urls = paste0(url,years,"/",ftp_files), output_file_paths)
				runtime = round(as.numeric(difftime(Sys.time(), start_tm, units = "mins")),2) # record run time
				cat(paste0("Download complete in ",runtime," mins!\n\n"))
				closeAllConnections()
				
				# # Loop across files to download sequentially (slower)...
				# for(ii in seq_along(ftp_files)) {
				# 	if(file.exists(output_file_paths[ii])) {
				# 		cat(paste0("File '",ftp_files[ii],"' already exists; skipping...\n\n"))
				# 	} else {
				# 		cat(paste0("Downloading '",ftp_files[ii],"' (file ", ii, " of ", length(ftp_files), " for ",years[i],")... "))
				# 		curl::curl_download(url = paste0(url,years[i],"/",ftp_files), output_file_paths[ii])
				# 		cat("Complete!\n\n")
				# 		closeAllConnections()
				# 	}
				# }
			}
		}
	} else stop("Invalid type! Please use one of the following: annual, monthly, daily, climatology")
}



# Usage:
download_netcdf_files(output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data/raw", 
					  type = "daily", years = 1989:2023, measure = "sst")
14*35/60 # about 8 hours to complete!


finish = Sys.time()
(runtime=(finish - start))



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
