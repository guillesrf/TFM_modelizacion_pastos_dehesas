library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(terra)
library(tidyterra)
library(ggplot2)

dir <- 'C:/TFM/ScriptsR/3_varExp_otras'
setwd(dir)


#######################
# Extraer los valores en cada punto
#######################

# Load AOI and extract names
aoi <- vect("./1_DATA/aoi/coords_parcelas.shp")
aoi_names <- as.data.frame(aoi)


# Add an index column to aoi_names for the join
aoi_names <- aoi_names %>%
  mutate(ID = row_number())

# List raster files
climate_files <- list.files('C:/TFM/wget_chelsa/envicloud/', pattern = "*.tif", recursive = TRUE, full.names = TRUE)



# Initialize an empty list to store results
results_list <- list()

# Loop through each file and extract values
for (f in 1:length(climate_files)) {
  r_name <- climate_files[f]
  raster <- rast(climate_files[[f]])
  
  # Extract values at the AOI points
  extracted_values <- terra::extract(x = raster, y = aoi)
  
  # Create a dataframe with the file name and extracted values
  df <- data.frame(r_name = r_name, extracted_values)
  
  # Rename the extracted value column to "values"
  value_col <- colnames(df)[ncol(df)]
  df <- df %>% rename(values = all_of(value_col))
  
  # Append the dataframe to the results list
  results_list[[f]] <- df
}

# Combine all dataframes in the list into a single dataframe
final_df <- bind_rows(results_list)

# Replace AOI IDs with names
final_df <- final_df %>%
  left_join(aoi_names, by = c("ID" = "ID")) %>%
  select(r_name, Name, values) %>%
  rename(aoi_name = Name)

# Extract file name and date

final_df <- final_df %>%
  mutate(r_name = str_extract(r_name, "[^/]+$")) %>%
  mutate(r_name = str_remove(r_name, "V2.1.tif|\\.tif$"))

# Create a new column "date" with the pattern "08_2004"
final_df <- final_df %>%
  mutate(date = str_extract(r_name, "\\d{2}_\\d{4}"))


#####################
# Guardar los datos climaticos en bruto
####################

#HISTORIC

historic_climate <- final_df %>%
  filter(!str_detect(r_name, "ssp"))


historic_climate <- historic_climate %>%
  mutate(variable = str_extract(r_name, "(?<=_)[^_]+"))

write.csv(historic_climate,paste0(dir,'/2_RESULTS/historic_climate_all.csv'))



#FUTURE
future_climate <- final_df %>%
filter(str_detect(r_name, "ssp"))

future_climate <- future_climate %>%
  mutate(variable = str_extract(str_remove(r_name, ".*ssp"), "(?<=_)[^_]+"))
future_climate <- future_climate %>%
  mutate(SSP = str_extract(r_name, "ssp\\d{3}"))


future_climate_ssp585 <- future_climate[future_climate$SSP == "ssp585",]

write.csv(future_climate_ssp585,paste0(dir,'/2_RESULTS/future_climate_all_spp585.csv'))

###################
# Procesar los datos climaticos y guardar csv

# pr a mm scale=0.1; pr_real = pr * 0.01
# tas a ºC; tas_real = (tas*0.1)-273.15

## HISTORICOS
var_clim_H <- historic_climate %>% mutate(Year = as.factor(substr(date,4,7)),
                                          Month = as.numeric(substr(date,1,2)),
                                          Season = case_when(Month >= 3  & Month <= 5 ~ "spring"),
                                          values = case_when( #reescalado de los valores
                                            variable == "pr" ~ values * 0.01, 
                                            variable %in% c("tas", "tasmin", "tasmax") ~ (values * 0.1) - 273.5, TRUE ~ values)  
) %>%
  #extraemos datos climaticos mean, max y min anuales y estacionales
  group_by(aoi_name, Year) %>%
  summarize(
    # Jan to Jun summaries (elapsed Year)
    #tas ; maximas/min y medias mensuales
    tas_max_year = mean(values[variable == "tasmax" & Month <= 6], na.rm = TRUE), #bio5
    tas_min_year = mean(values[variable == "tasmin" & Month <= 6], na.rm = TRUE), #bio6
    tas_mean_year = mean(values[variable == "tas" & Month <= 6], na.rm = TRUE),
    # bio 4 & 7
    bio4_year = sd(values[variable == "tas" & Month <= 6], na.rm = TRUE), #bio4
    bio7_year = min(values[variable == "tasmin" & Month <= 6],na.rm=TRUE)-max(values[variable == "tasmax" & Month <= 6],na.rm=TRUE), #bio 7
    # pr ; maximas/min y medias mensuales
    pr_max_year = max(values[variable == "pr" & Month <= 6], na.rm = TRUE),
    pr_min_year = min(values[variable == "pr" & Month <= 6], na.rm = TRUE),
    pr_mean_year = mean(values[variable == "pr" & Month <= 6], na.rm = TRUE),
    #acumuladas
    pr_accum_year = sum(values[variable == "pr" & Month <= 6]), #bio12
    #bio15
    bio15_year = sd(values[variable == "pr" & Month <= 6], na.rm = TRUE)/pr_mean_year*100, #bio15
    
    # Spring summaries (mar to jun)
    #maximas/min y medias mensuales
    tas_max_spring = mean(values[Season == "spring" & variable == "tasmax"], na.rm = TRUE), #bio5
    tas_min_spring = mean(values[Season == "spring" & variable == "tasmin"], na.rm = TRUE), #bio6
    tas_mean_spring = mean(values[Season == "spring" & variable == "tas"], na.rm = TRUE),
    # bio 4 & 7
    bio4_spring = sd(values[Season == "spring" & variable == "tas"], na.rm = TRUE), #bio4
    bio7_spring = min(values[variable == "tasmin" & Season == "spring"],na.rm=TRUE)-max(values[variable == "tasmax" & Season == "spring"],na.rm=TRUE), #bio 7
    #maximas/min y medias mensuales
    pr_max_spring = max(values[Season == "spring" & variable == "pr"], na.rm = TRUE), #mensual
    pr_min_spring = min(values[Season == "spring" & variable == "pr"], na.rm = TRUE), #
    pr_mean_spring = mean(values[Season == "spring" & variable == "pr"], na.rm = TRUE),
    #acumuladas
    pr_accum_spring = sum(values[variable == "pr" & Month >= 3 & Month <= 6]), #bio12
    #bio 15
    bio15_spring = sd(values[variable == "pr" & Month <= 6], na.rm = TRUE)/pr_mean_spring*100
  ) %>%
  mutate(plotname = paste0(aoi_name,"_",as.character(Year))) 

write.csv(var_clim_H,paste0(dir,'/2_RESULTS/var_clim_historic.csv'))

###########          
##FUTUROS
##########                 

## SSP585
var_clim_ssp585 <- future_climate_ssp585 %>% mutate(Year = as.factor(substr(date,4,7)),
                                                    Month = as.numeric(substr(date,1,2)),
                                                    Season = case_when(Month >= 3  & Month <= 5 ~ "spring")
) %>%
  #extraemos datos climaticos mean, max y min anuales y estacionales
  group_by(aoi_name, Year) %>%
  summarize(
    # Jan to Jun summaries (elapsed Year)
    #tas ; maximas/min y medias mensuales
    tas_max_year = mean(values[variable == "tasmax" & Month <= 6], na.rm = TRUE), #bio5
    tas_min_year = mean(values[variable == "tasmin" & Month <= 6], na.rm = TRUE), #bio6
    tas_mean_year = mean(values[variable == "tas" & Month <= 6], na.rm = TRUE),
    # bio 4 & 7
    bio4_year = sd(values[variable == "tas" & Month <= 6], na.rm = TRUE), #bio4
    bio7_year = min(values[variable == "tasmin" & Month <= 6],na.rm=TRUE)-max(values[variable == "tasmax" & Month <= 6],na.rm=TRUE), #bio 7
    # pr ; maximas/min y medias mensuales
    pr_max_year = max(values[variable == "pr" & Month <= 6], na.rm = TRUE),
    pr_min_year = min(values[variable == "pr" & Month <= 6], na.rm = TRUE),
    pr_mean_year = mean(values[variable == "pr" & Month <= 6], na.rm = TRUE),
    #acumuladas
    pr_accum_year = sum(values[variable == "pr" & Month <= 6]), #bio12
    #bio15
    bio15_year = sd(values[variable == "pr" & Month <= 6], na.rm = TRUE)/pr_mean_year*100, #bio15
    
    # Spring summaries (mar to jun)
    #maximas/min y medias mensuales
    tas_max_spring = mean(values[Season == "spring" & variable == "tasmax"], na.rm = TRUE), #bio5
    tas_min_spring = mean(values[Season == "spring" & variable == "tasmin"], na.rm = TRUE), #bio6
    tas_mean_spring = mean(values[Season == "spring" & variable == "tas"], na.rm = TRUE),
    # bio 4 & 7
    bio4_spring = sd(values[Season == "spring" & variable == "tas"], na.rm = TRUE), #bio4
    bio7_spring = min(values[variable == "tasmin" & Season == "spring"],na.rm=TRUE)-max(values[variable == "tasmax" & Season == "spring"],na.rm=TRUE), #bio 7, #bio 7
    #maximas/min y medias mensuales
    pr_max_spring = max(values[Season == "spring" & variable == "pr"], na.rm = TRUE), #mensual
    pr_min_spring = min(values[Season == "spring" & variable == "pr"], na.rm = TRUE), #
    pr_mean_spring = mean(values[Season == "spring" & variable == "pr"], na.rm = TRUE),
    #acumuladas
    pr_accum_spring = sum(values[variable == "pr" & Month >= 3 & Month <= 6]), #bio12
    #bio 15
    bio15_spring = sd(values[variable == "pr" & Month <= 6], na.rm = TRUE)/pr_mean_spring*100
  ) %>%
  mutate(plotname = paste0(aoi_name,"_",as.character(Year))) 

write.csv(var_clim_ssp585,paste0(dir,'/2_RESULTS/var_clim_ssp585.csv')) 
###################
