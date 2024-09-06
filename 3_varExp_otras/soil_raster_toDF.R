library(tidyverse)
library(lubridate)
library(stringr)
library(terra)
library(sf)
library(tidyterra)
library(ggplot2)

dir <- 'C:/TFM/ScriptsR/3_varExp_otras'
setwd(dir)

#######################
# Extraer los valores en cada punto
#######################


# Load AOI and extract names
aoi <- vect("./1_DATA/aoi/coords_parcelas.shp")
aoi <- project(aoi,'EPSG:25829')

# Add an index column to aoi_names for the join
aoi_names <- as.data.frame(aoi)
aoi_names <- aoi_names %>%
  mutate(ID = row_number())


# List raster files
# previamente hay que convertir los .rst de propeidades fisicas a tif para que funcione bien
soil_files <- list.files(paste0(dir,'/1_DATA/soil_reprojected/'), pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# Initialize an empty list to store results
results_list <- list()

#test

for (f in seq_along(soil_files)) {
  r_name <- soil_files[f]
  raster <- rast(r_name)
  #raster <- project(raster,'epsg:25829')
  
  # Check overlap before extraction
  if (!is.null(intersect(ext(raster), ext(aoi)))) {
    # Extract values at the AOI points
    extracted_values <- terra::extract(x = raster, y = aoi)
    
    # Only proceed if extraction is successful and not empty
    if (nrow(extracted_values) > 0) {
      # Create a dataframe with the file name and extracted values
      df <- data.frame(r_name = r_name, extracted_values)
      
      # Rename the extracted value column to "values"
      value_col <- colnames(df)[ncol(df)]
      df <- df %>% rename(values = all_of(value_col))
      
      # Append the dataframe to the results list
      results_list[[f]] <- df
    } else {
      print(paste("No values extracted for raster:", r_name))
    }
  } else {
    print(paste("Raster does not overlap with AOI:", r_name))
  }
}

# Combine all dataframes in the list into a single dataframe
final_df <- bind_rows(results_list)

rm(raster)


final_df_save <- final_df
# Replace AOI IDs with names
final_df <- final_df %>%
  left_join(aoi_names, by = c("ID" = "ID")) %>%
  rename(aoi_name = Name) %>%
  # separate_rows to split 'Periodo' by comma and create new rows
  separate_rows(Periodo, sep = ",") %>%
  # renombramos como Aoi_name + año
  mutate(plotname = paste0(aoi_name, "_", Periodo)) %>%
  mutate(soil_variable = str_extract(r_name, "(?<=HU29_)[^/]+(?=\\.tif)"))
#eliminamos campos innecesarios
final_df <- final_df[,-c(1,2,4:7)] %>% 
  #pivot_wider para que cada variable sea una columna
  pivot_wider(names_from = soil_variable, values_from = values)





#####################
# guardamos los datos
####################


write.csv(final_df,paste0(dir,'/2_RESULTS/var_soil.csv'))

