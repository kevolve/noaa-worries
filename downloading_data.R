#### Download netCDF files from NOAA CoralWatch FTP server ####
#### (c) Kevin Novak, AIMS
#### Updated 7 May 2025

# TODO: Emojify cat() messages!
# TODO: Check for previous saves, verify download with checksums, save only for files not properly downloaded

#### Install dependencies ####

# Install and load the required packages (if not installed previously):
if (!require("tidyverse")) {install.packages("tidyverse"); require(tidyverse)}
if (!require("purrr")) {install.packages("purrr"); require(purrr)}
if (!require("ncdf4")) {install.packages("ncdf4"); require(ncdf4)}
if (!require("RCurl")) {install.packages("RCurl"); require(RCurl)}


#### Set options ####

# Output path location must have at least ~150 GB of space for downloaded SST data:
output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data"

# years can range from 1985-2023, be a single year, or multiple separated by a colon
years = 1988:2025 # all available years

# timeframe is one of: "annual", "monthly", or "daily"
timeframe = "daily"

# measure is one of: "sst", "baa", "baa-max-7d", "dhw", "hs", "ssta", "sst-trend-7d", "year-to-date_2022", "year-to-date", "climatology"
# see https://github.com/ecolology/noaa-worries/blob/main/README.md for abbreviation definitions
measure = "sst"

output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data"
years = c(1992,1999)
measure = "ssta"
summary_type = c("min")
timeframe = "monthly"

output_path = "/Users/uqkbairo/MODRRAP/storage1tb/data"
years = c(1992,1999)
measure = "baa"
summary_type = c("max")
timeframe = c("annual", "climatology", "monthly", "daily")

#### download_netcdf_files ####

# Define the function for downloading netCDF files from the NOAA server:
download_netcdf_files <- function(
		output_path = "data/raw/",
		years = NULL,
		measure = c("climatology", "baa", "baa5", "dhw", "hs", "sst", "ssta"),
		summary_type = "all",
		timeframe = c("annual", "monthly", "daily", "climatology")) #
{
	# from: https://www.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/
	# baa = bleaching alert area; sst = sea surface temperature; ssta = SST anomaly
	# hs = hot spot; dhw = degree heating weeks
	
	require(tidyverse)
	require(purrr)
	require(ncdf4)
	require(RCurl)
	
	# Custom functions:
	check_path <- function(file) {
		if(stringr::str_detect(file, regex(".*\\..*"))) path = dirname(file) else path = file
		if(stringr::str_sub(path,-1,-1) != "/") path = paste0(path, "/")
		if(!file.exists(path)) {dir.create(path, recursive = TRUE); cat(paste0("Created path ", path, "\n"))}
		return(path)
	}
	list_ftp_files <- function(url) {
		RCurl::getURL(url, ftp.use.epsv = FALSE, crlf = TRUE, dirlistonly = TRUE) %>% 
			str_split(pattern="\n", simplify = TRUE) %>% 
			as.character() %>% 
			keep(str_detect(., "\\S")) %>%
			sort()
	}
	# Use this function to verify that a specific file has downloaded alright!
	checksum <- function(file_path, rm=T) {
		md5_file_path = paste0(file_path, ".md5")
		if(file.exists(md5_file_path)) {
			noaa_checksums <- 
				system(paste("cat", md5_file_path), intern = TRUE) %>%
				str_extract(regex("^\\w*"))
			my_checksums <- 
				system(paste0("md5sum ",file_path), intern = TRUE) %>%
				str_extract(regex("^\\w*"))
			if(!all.equal(noaa_checksums, my_checksums)){
				warning(basename(file_path), " does not match checksums! Deleting...\n")
				file.remove(file_path, md5_file_path)	
			}
		}
	}
	`%nins%` <- function(x, y) !all(sapply(x, `%in%`, y)) # "not in's" for checking if not all x are in y	
	
	# Conditional checks
	if(length(measure) > 1) stop("Can only download one measure at a time currently!")
	
	if(measure != "climatology" & timeframe %nins% c("annual", "monthly", "daily", "climatology"))
		stop("Invalid timeframe(s)! Please use one (or multiple) of the following:\nannual, monthly, daily, climatology")
	
	# Possible daily, monthly, and annual measures:
	daily_measures <- c("baa", "baa-max-7d", "baa5", "baa5-max-7d", "dhw", "hs", "sst", "sst-trend-7d", "ssta", "year-to-date")
	monthly_measures <- c("baa", "baa5", "dhw", "hs", "sst", "ssta")
	annual_measures <- c("baa", "baa5", "dhw", "hs", "sst", "ssta")
	
	if(("climatology" %in% timeframe & measure != "sst") | measure == "climatology") cat("Note: Climatology is only available as sst (°C)!")
	
	if("daily" %in% timeframe & measure %nins% c(daily_measures, "climatology")) 
		stop(paste0("Invalid measure! Try one of the following:\n",paste0(daily_measures, collapse = ", "),"\n\nExplore the directory for yourself at:\nhttps://www.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/"))
	
	if("monthly" %in% timeframe & measure %nins% c(monthly_measures, "climatology")) 
		stop(paste0("Invalid measure! Try one of the following:\n",paste0(monthly_measures, collapse = ", "),"\n\nExplore the directory for yourself at:\nhttps://www.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/nc/v1.0/monthly/"))
	
	if("annual" %in% timeframe & measure %nins% c(annual_measures, "climatology")) 
		stop(paste0("Invalid measure! Try one of the following:\n",paste0(annual_measures, collapse = ", "),"\n\nExplore the directory for yourself at:\nhttps://www.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/nc/v1.0/annual/"))
	
	if(is.null(summary_type)) summary_type = "all"
	if(summary_type %nins% c("all", "mean", "min", "max"))
		stop("Invalid summary_type! Try one of the following, or do not assign: all, mean, min, max")
	
	if(measure == "climatology") {timeframe = climatology; warning('timeframe overwritten to "climatology", given measure = "climatology"')}
	if(any(timeframe != "climatology") ) {
		if(is.null(years))	stop(paste0("Must select year(s) from 1985 to present for ",measure,"!"))
		if(years %nins% 1985:2025) 	stop(paste0("Must select year(s) from 1985 to present for ",measure,"!"))
	}
	
	
	# Check output path
	cat("Retrieving filenames from server...\n")
	base_url = "ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/"
	output_path <- check_path(output_path) # fixes path with slash at end (if not already)
	
	# create list of paths for each timeframe
	timeframe_paths <- c()
	for(i in 1:length(timeframe)){
		if(timeframe[i] == "climatology") {
			part_path <- "climatology"
		} else {
			part_path <- paste0(measure,"/",timeframe[i])
		}
		timeframe_paths[i] <- check_path(paste0(str_remove(output_path, paste0(part_path,"/$")),paste0(part_path,"/")))
	}
	
	# Output messages
	{
		if(length(timeframe) == 1) {
			message(paste0("Data download path set to: \n", timeframe_paths),"\n")
			message(paste0("Measure: ",measure))
			message(paste0("Timeframe: ",timeframe))
		} else {
			message(paste0("Data download paths set to: \n", paste0(timeframe_paths, collapse="\n"),"\n"))
			message(paste0("Measure: ",measure))
			message(paste0("Timeframes: ",paste0(timeframe, collapse = ", ")))
		}
		if(length(years) == length(min(years):max(years))){
			message(paste0("Years: ", min(years), ":", max(years)))
		} else if(length(years) == 1) {
			message(paste0("Year: ", years))
		} else {
			message(paste0("Years: ", paste0(years, collapse=",")))
		}
		if(length(summary_type) == 1) {
			message(paste0("Summary type: ", summary_type, "\n"))
		} else {
			message(paste0("Summary types: ", paste0(years, collapse=","), "\n"))
		}
	} # end printing
	
	url_file_paths <- save_file_paths <- c() # initialise empty file_path vecs
	
	if("climatology" %in% timeframe) {
		
		cat(paste0("Retrieving climatology filename...\n"))
		folder_url <- paste0(base_url, "climatology/")
		filenames <- list_ftp_files(folder_url) %>% str_subset(".*.nc")
		url_file_paths <- c(url_file_paths, paste0(folder_url, filenames))
		save_file_paths <- c(save_file_paths, paste0(timeframe_paths[timeframe == "climatology"], filenames))
		if(length(filenames) > 1) stop("'/climatology/' on FTP server contains multiple .nc files!")
		
	} 
	
	
	if("annual" %in% timeframe) {
		
		# Determine folder and filenames:
		cat(paste0("Retrieving annual filenames...\n"))
		folder_url <- paste0(base_url, "nc/v1.0/annual/")
		filenames <- list_ftp_files(folder_url)
		
		# Keep only some summary_types (if supplied):
		if(summary_type[1] != "all") {
			possible_summary_types <- filenames %>% str_subset(paste0("^ct5km_",measure,".*$")) %>% str_remove("^.*-") %>% str_remove("_.*$") %>% unique
			file_i <- filenames %>% 
				str_which(paste0("^ct5km_", measure,"-",summary_type, "_v3.1_.*$", collapse="|"))
			filenames <- filenames[file_i]
		}
		
		# Assign full url and save paths for curl download:
		if(length(filenames) == 0) {
			warning(paste0("Summary type(s): '", paste0(summary_type, collapse=", "), "' returned zero results, try instead: ", paste0(possible_summary_types, collapse=", ")))
		} else {
			url_file_paths <- c(url_file_paths, paste0(folder_url, filenames))
			save_file_paths <- c(save_file_paths, paste0(timeframe_paths[timeframe == "annual"], filenames))
		}
	} # end annual
	
	if("monthly" %in% timeframe) {
		
		# Determine folders and filenames:
		folder_urls <- paste0(base_url, "nc/v1.0/monthly/",years,"/")
		
		for(i in 1:length(folder_urls)) {
			cat(paste0("Retrieving monthly filenames for ",years[i],"...\n"))
			filenames <- list_ftp_files(folder_urls[i])
			
			# Keep only some summary_types (if supplied):
			if(summary_type[1] != "all") {
				file_i <- filenames %>% 
					str_which(paste0("^ct5km_", measure,"-",summary_type, "_v3.1_.*$", collapse="|"))
				filenames <- filenames[file_i]
			}
			
			# Assign full url and save paths for curl download:
			if(length(filenames) == 0) {
				warning(paste0("Summary type(s): '", paste0(summary_type, collapse=", "), "' returned zero results, try instead: ", paste0(possible_summary_types, collapse=", ")))
			} else {
				url_file_paths <- c(url_file_paths, paste0(folder_url, filenames))
				save_file_paths <- c(save_file_paths, paste0(timeframe_paths[timeframe == "monthly"], filenames))
			}
		} # yearly for-loop
	} # end monthly
	
	if("daily" %in% timeframe) {
		
		# Determine folders and filenames:
		folder_urls <- paste0(base_url, "nc/v1.0/daily/",measure,"/",years,"/")
		
		for(i in 1:length(folder_urls)) {
			cat(paste0("Retrieving daily filenames for ",years[i],"...\n"))
			filenames <- list_ftp_files(folder_urls[i])
			
			# Assign full url and save paths for curl download:
			url_file_paths <- c(url_file_paths, paste0(folder_url, filenames))
			save_file_paths <- c(save_file_paths, paste0(timeframe_paths[timeframe == "daily"], filenames))
		} # yearly for-loop
	} # end daily
	
	
	# Check if files have already been downloaded...
	
	# Checksums:
	nc_file_paths <- save_file_paths %>% str_subset(".*.nc$")
	md5_file_paths <- paste0(nc_file_paths,".md5")
	for(i in 1:length(nc_file_paths)) {
		checksum(nc_file_paths[i])
	}
	
	extant_files_i <- c()
	for(i in 1:length(save_file_paths)) {
		if(file.exists(save_file_paths[i])){
			extant_files_i <- c(extant_files_i, i)
		}
	}
	url_file_paths <- url_file_paths[-extant_files_i]
	save_file_paths <- save_file_paths[-extant_files_i]
	
	if(length(save_file_paths) > 0) {
		cat(paste0("Starting main download... ", length(save_file_paths), " files to download\n")) # line not working for some reason?
		
		closeAllConnections()
		start_tm = Sys.time() # record start time
		
		curl_downloads <- curl::multi_download(urls = url_file_paths, destfiles = save_file_paths, resume = TRUE)
		curl_errors <- curl_downloads$error[!is.na(curl_downloads$error)];
		if(any(curl_errors != "")) {
			curl_errors <- curl_errors[curl_errors != ""]
			cat(paste0(length(curl_errors), " file(s) failed to download with the following error(s): \n", paste0(curl_errors, collapse = "\n"), "\n\n"))
		}
		cat("Download complete!\n")
		
		
		# Checksums:
		cat("Completing checksums...\n")
		nc_file_paths <- save_file_paths %>% str_subset(".*.nc$")
		md5_file_paths <- paste0(nc_file_paths,".md5")
		for(i in 1:length(nc_file_paths)) {
			checksum(nc_file_paths[i])
		}
	} else { 
		cat(paste0("All files for the year '",years[i], "' are already downloaded!\n\n"))
		
		runtime = round(as.numeric(difftime(Sys.time(), start_tm, units = "mins")),2) # record run time
		cat(paste0("Full download completed in ",runtime," mins!\n\n"))
		
		
	} # length is non-zero check
	
	closeAllConnections()
	return(T) # If we made it to the end, return TRUE
}



#### Usage ####

# The following code runs the download function, storing files in the output location
download_netcdf_files(output_path, 
					  years = 1985, 
					  timeframe = c("daily", "monthly", "annual", "climatology"), 
					  measure = "sst",
					  summary_type = "mean")
# with good internet connection ~14 mins to download one year's daily temperature data (365 files)
# so ~9hr for all files


download_netcdf_files(output_path, 
					  years = 2023:2025, 
					  timeframe = "annual", 
					  measure = "dhw")


#### Troubleshooting ####

# IF you find that some of the files had errors in downloading 
# (often much smaller file size, and cannot be extracted from), 
# re-try download again by deleting these files and re-running 
# the above function for those years - it will scan for files
# that already exist and skip them.
