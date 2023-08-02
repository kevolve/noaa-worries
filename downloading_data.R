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



# # Testing:
# output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data/raw"
# type = "daily"
# years = 1988:2023 # all available years
# measure = "sst"


download_netcdf_files <- function(
		output_path = "data/raw/",
		years = 2023,
		type = "daily", # c("annual", "monthly", "daily"),
		measure = "sst") #c("sst", "baa", "baa-max-7d", "dhw", "hs", "ssta", "sst-trend-7d", "year-to-date_2022", "year-to-date", "climatology")) { 
	{ 
	
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
				
				if(!file.exists(paste0(output_path,"/",measure))) stop("File path '",paste0(output_path,measure),"' does not exist!\nCannot begin download!")
				
				output_file_dir <-  paste0(output_path,measure,"/",years[i],"/")
				if(!file.exists(output_file_dir)) dir.create(output_file_dir) # create the year's file if it doesn't exist already
				existing_files <- list.files(path = output_file_dir)
				ftp_files <- ftp_files[!(ftp_files %in% existing_files)]
				if(length(ftp_files) > 0) {
					
					output_file_paths <- paste0(output_file_dir, ftp_files)
					input_urls <- paste0(url,years[i],"/",ftp_files)
					
					# Download all files using curl's multi_download (in parallel; allows for resuming of large files)
					cat(paste0("Starting year ",years[i], " download... (",length(ftp_files)," files)\n")) # line not working for some reason?
					# cat(paste0("Starting year download...\n")) # line not working for some reason?
					start_tm = Sys.time() # record start time
					x <- curl::multi_download(urls = input_urls, destfiles = output_file_paths)
					x_err <- x$error[!is.na(x$error)];
					if(any(x_err != "")) {x_err <- x_err[x_err != ""]
					cat(paste0(length(x_err), " file(s) failed to download with the following error(s): \n", paste0(x_err,collapse = "\n"), "\n"))}
					runtime = round(as.numeric(difftime(Sys.time(), start_tm, units = "mins")),2) # record run time
					cat(paste0("Download complete in ",runtime," mins!\n\n"))
					closeAllConnections()
					
				} else { cat(paste0("All files for the year '",years[i], "' are already downloaded!\n\n"))}
				
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
					  years = 1985:1988, type = "daily", measure = "sst")
14*length(1985:2023)/60 # with good internet connection ~14mins per year (365 files), so ~9hr for all files

# IF you see that some of the years had errors in downloading the files, 
# try again by re-running the above function - it will scan for files
# that already exist and skip them.
