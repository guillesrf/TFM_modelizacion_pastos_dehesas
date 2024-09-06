library(gribr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(terra)
library(tidyterra)

dir <- 'C:/TFM/ScriptsR/3_varExp_otras'
setwd(dir)


#https://gis.stackexchange.com/questions/396408/how-to-properly-extract-point-data-from-multi-raster-grib-file-in-r

#datos de era 5 res 0.1x0.1ºC
clim_raster <- rast('C:/TFM/ERA5/spain_monthly/data.grib')
band_names <- names(clim_raster)
raster_dates <- terra::time(clim_raster)


#aoi
aoi <- vect("./1_DATA/aoi/coords_parcelas.shp")
aoi_names <- as.data.frame(aoi)
aoi_names <- aoi_names %>%
  mutate(ID = row_number())

# Initialize an empty list to store results
extracted_data <- list()

# Loop through each layer (band) and extract values for the AOI
for (i in seq_along(band_names)) {
  # Extract the values for the current band
  values <- terra::extract(clim_raster[[i]], aoi, df = TRUE)
  
  # Add band name and time to the extracted data
  values$band_name <- band_names[i]
  values$date <- raster_dates[i]
  names(values) <- c('ID','values','band_name','date')
  # Append to the list
  extracted_data[[i]] <- values
}

# Combine all extracted data into a single data frame
final_data <- do.call(rbind, extracted_data)

#Añadimso el nombre de cada parcela
# Replace AOI IDs with names
final_data <- final_data %>%
  left_join(aoi_names, by = c("ID" = "ID"))
final_data <- final_data[,-c(5,6,8)]
final_data <- final_data %>% 
  pivot_wider(names_from = band_name, values_from = values)

###ESTA EN KELVIN!!
