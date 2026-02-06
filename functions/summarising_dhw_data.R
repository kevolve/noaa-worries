#### Install dependencies ####

install.packages(c("raster", "ncdf4"),
                 type = "source")


# Install and load the required packages (if not installed previously):
library(ncdf4)
library(terra)
library(sf)
library(arrow)
library(dplyr)
library(purrr)

sf::sf_extSoftVersion()


nc_save_path <- "/Users/uqkbairo/MODRRAP/storage1tb/data/raw/dhw/annual/"
nc_files <- list.files(nc_save_path,"\\.nc$", full.names = T)
file_info <- str_match(basename(nc_files), "^ct5km_(.{3,4})-(.{3,4})_v3.1_(.{4})\\.nc$") %>%
    as.data.frame() %>% `colnames<-`(c("file_name", "measure", "summary_type", "year")) %>%
    mutate(year = as.numeric(year))
parquet_dir <- paste0("/Users/uqkbairo/MODRRAP/storage1tb/data/dhw/annual/dhw_annual_", min(file_info$year), "-", max(file_info$year),"_parquet")


# r <- terra::rast(nc_files[1], subds = "degree_heating_week")
# plot(r)


target <- coords
out <- data.frame()
for (f in 1:length(nc_files)) {
    cat("Writing file", f,"\n")
    r <- rast(nc_files[f], subds = "degree_heating_week")
    
    out <- rbind(out, 
                 data.frame(year = file_info$year[f], 
                            reef = target$reef,
                            dhw = terra::extract(r, st_transform(target, st_crs(r)))$degree_heating_week))
    out_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE) %>%
        mutate(year = file_info$year[f], .before = 1L)
    
    arrow::write_dataset(
        out_df,
        path = parquet_dir,
        format = "parquet",
        partitioning = "year",
        existing_data_behavior = "overwrite"
    )
}


out %>%
    ggplot(aes(x = year, y = dhw)) + 
    facet_wrap(~reef) +
    geom_line()


write.csv(out, file = "dhws_for_t.csv", row.names = F)

