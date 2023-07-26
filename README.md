# No worries getting sea temperature data with noaa-worries!
**A repo to help with downloading and processing NOAA temperature data**

This repo is *currently under development* but will include helpful scripts and functions for downloading and summarising overnight temperature data from NOAA's CoralTemp FTP server. 
Download NOAA data products such as:
* Nighttime sea surface temperatures (SST; *sst*)
* Past year-to-date temperatures (for 2022; *year-to-date_2022*) and current year-to-date temperatures (for 2023; *year-to-date*)
* SST anomalies (*ssta*)
* SST 7-day trends (*sst-trend-7d*)
* Bleaching alert area (*baa*) and 7-day maximum BAA (baa-max-7d)
* Maximum monthly mean (MMM) climatology data (a.k.a. baseline or reference data) from 1985-1990+1993 (*climatology*)
* Daily hot spot (°C above MMM; *hs*)
* Degree heating weeks (DHW - the cumulative hot spot temperatures across 3 months exceeding MMM+1°C; *dhw*)

It (hopefully) will include the following features:
* Download NetCDF files to a specified output location
* Convert NetCDF files to GeoTiffs
* Summarise Degree Heating Weeks across time for given locations
* Interpolate temperatures when reefs occur across multiple 5km grid cells
