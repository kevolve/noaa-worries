# Need sea surface temperature data? NO(AA) worries!
**A repo to help with downloading and processing NOAA temperature data from their FTP server**

<img src="plots/moore_reef_temp_animated.gif" alt="moore_reef_sst" width="500"/>


This repo is *under active development* but includes helpful scripts and functions for downloading and summarising nighttime sea surface temperature data from NOAA's CoralTemp FTP server.

Get NOAA data products such as:
* Nighttime sea surface temperatures (SST; *sst*)
* Past year-to-date temperatures (for 2022; *year-to-date_2022*) and current year-to-date temperatures (for 2023; *year-to-date*)
* SST anomalies (*ssta*)
* SST 7-day trends (*sst-trend-7d*)
* Bleaching alert area (*baa*) and 7-day maximum BAA (baa-max-7d)
* Maximum monthly mean (MMM) climatology data (a.k.a. baseline or reference data) from 1985-1990+1993 (*climatology*)
* Daily hot spot (°C above MMM; *hs*)
* Degree heating weeks (DHW - the cumulative hot spot temperatures across 3 months exceeding MMM+1°C; *dhw*)

It includes the following scripts:
* Download complete NetCDF files for the above NOAA data products to a specified output location
* Extract SSTs from downloaded NetCDF files using site coordinates
* Mannually re-compute NOAA products such as Degree Heating Weeks for specific locations

If you are interested in a feature or function, feel free to submit a feature request via 'Issues'!
