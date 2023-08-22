#### Downloading netCDF files from FTP server ####
#### (c) Kevin Novak, University of Queensland
#### Updated 9 Aug 2023



#### Install dependencies ####

# Install and load the required packages (if not installed previously):
if (!require("tidyverse")) {install.packages("tidyverse"); require(tidyverse)}
if (!require("purrr")) {install.packages("purrr"); require(purrr)}
if (!require("ncdf4")) {install.packages("ncdf4"); require(ncdf4)}
if (!require("RCurl")) {install.packages("RCurl"); require(RCurl)}


#### Set options ####

# Output path location must have at least ~150 GB of space for downloaded SST data:
my_download_path = "/Users/uqkbairo/MODRRAP/storage1tb"

# years can range from 1985-2023, be a single year, or multiple separated by a colon
years = 1988:2023 # all available years

# type is one of: "annual", "monthly", or "daily"
type = "daily"

# measure is one of: "sst", "baa", "baa-max-7d", "dhw", "hs", "ssta", "sst-trend-7d", "year-to-date_2022", "year-to-date", "climatology"
# see https://github.com/ecolology/noaa-worries/blob/main/README.md for abbreviation definitions
measure = "sst"


#### download_netcdf_files ####

# Define the function for downloading netCDF files from the NOAA server:
download_netcdf_files <- function(
		output_path = "data/raw/",
		years = 2023,
		type = c("annual", "monthly", "daily"),
		measure = c("sst", "baa", "baa-max-7d", "dhw", "hs", "ssta", "sst-trend-7d", "year-to-date_2022", "year-to-date", "climatology")) #
	{ 
	
	# Use this function to verify that a specific file has downloaded alright!
	check_md5 <- function(file_path) {
		# file_path = "/Users/uqkbairo/MODRRAP/storage1tb/data/climatology/ct5km_climatology_v3.1.nc"
		md5_file_path = paste0(file_path, ".md5")
		if(file.exists(md5_file_path)) {
			orig_md5 = as.character(read.table(md5_file_path))[1]
		} else stop(".md5 file for this file does not exist!")
		file_md5 <- paste0("md5sum ",file_path) %>% # define CL command
			system(intern = TRUE) %>% # run checksums on file
			str_extract(regex("^\\w*")) # extract only the alphanumerics of md5
		if(all.equal(orig_md5, file_md5) == TRUE) {
			return(TRUE)
		} else {
			cat("Error with check_md5!")
			cat(all.equal(orig_md5, file_md5))
			return(FALSE)
		}
	}

	require(tidyverse)
	require(purrr)
	require(ncdf4)
	require(RCurl)
	
	# Custom functions:
	check_path <- function(path) {
		if(!file.exists(path)) stop(paste0("The path: '",path,"' does not exist or is incorrect and cannot be set! Please correct path and try again."))
		if(str_sub(path,-1,-1) == "/") out_path = path else out_path = paste0(path, "/")
		return(out_path)
	}
	list_ftp_files <- function(url) {
		require(RCurl)
		cat("Retrieving files...")
		RCurl::getURL(url, ftp.use.epsv = FALSE, crlf = TRUE, dirlistonly = TRUE) %>% 
			str_split(pattern="\n", simplify = TRUE) %>% 
			as.character() %>% 
			keep(str_detect(., "\\S"))
	}
	
	# Check output path
	output_path <- check_path(output_path)
	
	# Create the data/raw directory for saving 150GB of data (if it doesn't exist already):
	if(!file.exists(paste0(check_path(my_path),"data/"))) dir.create(paste0(check_path(my_path),"data/"))
	if(!file.exists(paste0(check_path(my_path),"data/raw/"))) dir.create(paste0(check_path(my_path),"data/raw")) # output files will be in the directory: 'data/raw/'
	
	output_path <- paste0(output_path,"data/raw/") # overwrite with new path
	
	# Output messages
	cat(paste0("Downloading data to the following path: '", output_path,"'\n"))
	if(length(type) > 1) {type = type[1]}; cat(paste0("Type: '",type,"'\n"))
	if(length(measure) > 1) {measure = measure[1]}; cat(paste0("Measure: '",measure,"'\n"))
	cat(paste0("Year(s): ", paste0(years, collapse=","),"\n\n"))
	
	
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
			}
		}
	} else stop("Invalid type! Please use one of the following: annual, monthly, daily, climatology")
}



#### Usage ####

# The following code runs the download function, storing files in the output location
download_netcdf_files(output_path = my_download_path, 
					  years = 1985:2023, 
					  type = "daily", 
					  measure = "sst")
# with good internet connection ~14 mins to download one year's daily temperature data (365 files)
# so ~9hr for all files


#### Troubleshooting ####

# IF you find that some of the files had errors in downloading 
# (often much smaller file size, and cannot be extracted from), 
# re-try download again by deleting these files and re-running 
# the above function for those years - it will scan for files
# that already exist and skip them.
