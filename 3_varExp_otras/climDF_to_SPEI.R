library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(terra)
library(sf)
library(SPEI)


dir <- 'C:/TFM/ScriptsR/3_varExp_otras'
setwd(dir)


historic <- read.csv('./2_RESULTS/historic_climate_all.csv')
future126 <- read.csv('./2_RESULTS/future_climate_all_spp126.csv')
future585 <- read.csv('./2_RESULTS/future_climate_all_spp585.csv') 

# Load AOI and extract names
aoi <- vect("./1_DATA/aoi/coords_parcelas.shp")
aoi_names <- as.data.frame(aoi) %>%
# # separate_rows to split 'Periodo' by comma and create new rows
# separate_rows(Periodo, sep = ",") %>%
#   # renombramos como Aoi_name + año
# mutate(plotname = paste0(Name, "_", Periodo)) %>%
mutate(plotname_future = paste0(Name, "_", '2041')) 



# LATITUDES 
aoi <- read_sf("C:/TFM/ScriptsR/3_varExp_otras/1_DATA/aoi/coords_parcelas.shp") 
st_crs(aoi) <- 25829
aoi <- st_transform(aoi, crs = 4326)
aoi_coords <- aoi %>%
  mutate(
    name = as.character(Name),  
    coords = st_coordinates(geometry)  # Extract coordinates
  ) %>%
  unnest(coords) %>%
  mutate (Y = round(coords[,2],2)) %>%
  select(name, Y)   # Separate coordinates into individual columns 
aoi_coords <- as.data.frame(aoi_coords)[,-3]
rm(aoi)

#######################################################
# HISTORIC
#######################################################

historic <- historic %>% 
  mutate(year = substr(date,4,7)) %>% #crear fecha ano
  mutate(month = as.numeric(substr(date,1,2))) %>%
  filter(variable %in% c("pr", "tasmin","tasmax")) %>% 
  mutate(season = case_when(month >= 3  & month <= 5 ~ "spring"),
         values = case_when( #reescalado de los valores
           variable == "pr" ~ values * 0.01, 
           variable %in% c("tas", "tasmin", "tasmax") ~ (values * 0.1) - 273.5, TRUE ~ values)) %>%
  mutate(plotname = paste0(aoi_name,"_",as.character(year))) %>% #nombre de parcela+año
  select(plotname, variable, values, year,month,season,aoi_name) %>%
  pivot_wider(
    names_from = variable,
    values_from = values
    ) %>%
  # inner_join(aoi_names, by = "plotname") %>% #filtrar por parcelas de estudio
  left_join( aoi_coords, by = c("aoi_name" = "name")) %>%  #añadir latitud
  # CALCULAR PET y CMI
  mutate(
    ET0 = hargreaves(Tmin = tasmin, 
                     Tmax = tasmax, 
                     lat = unique(Y)[1],  # Assuming `Y` contains latitude
                     Pre = pr)
  ) %>%
  mutate(cmi = pr - ET0) %>%
  mutate(Date = as.Date(paste(year, month, 1, sep = "-"), format = "%Y-%m-%d")) %>%
  mutate(pr = round(pr,2),tasmin=round(tasmin,2),tasmax=round(tasmax,2)) %>%
  mutate(plotname = paste0(aoi_name,"_",as.character(year))) #nombre de parcela+año
   
#ordenar CMI
split_historic <- split(historic, historic$aoi_name)

# Sort each dataframe by the 'date' column in ascending order
sorted_historic <- lapply(split_historic, function(df) {
  df %>% arrange(Date)
})
  
process_spei <- function(df) {
  # #convertir cmi en time series
  cmi_ts <- ts(df$cmi, start = c(min(df$year), min(df$month)), frequency = 12)
  
  # Calculate SPEI for 3-month and 5-month scales
  spei3 <- spei(cmi_ts, scale = 3)$fitted # primavera
  spei5 <- spei(cmi_ts, scale = 5)$fitted # desde enero
  spei8 <- spei(cmi_ts, scale = 8)$fitted #desde otoño
  spei12 <- spei(cmi_ts, scale = 12)$fitted # 1 ano de ref
  spei24 <- spei(cmi_ts, scale = 24)$fitted # 2 ano
  
  cmi3 <- zoo::rollsumr(df$cmi, k = 3, fill = NA, align = "right")
  cmi5 <- zoo::rollsumr(df$cmi, k = 5, fill = NA, align = "right")

  # Create a new dataframe to store the SPEI values
  spei_df <- df %>%
    select(aoi_name, year, month, Date) %>%
    mutate(spei3 = spei3, spei5 = spei5,spei8 = spei8,spei12 = spei12,spei24 = spei24,
           cmi3 = cmi3, cmi5=cmi5)
  
  return(spei_df)
}

# Apply the function to each dataframe in the list
historic_spei_results <- lapply(sorted_historic, process_spei)
# el problema es que faltan datos para los años entre medias 

may_spei_historic <- lapply(historic_spei_results, function(df) {
  df %>%
    filter(month == 5) %>%   # Filter for the month of May
    select(aoi_name, year, spei3, spei5,spei8,spei12,spei24,cmi3,cmi5)  # Select relevant columns
})

# Combine the results into a single dataframe if needed
var_spei_historic <- do.call(rbind, may_spei_historic)
var_spei_historic <- var_spei_historic %>%
  mutate(plotname = paste0(aoi_name,'_',year))

write.csv(var_spei_historic, paste0(dir,'/2_RESULTS/var_spei_historic.csv'))





#######################################################
# SSP126
#######################################################

ssp126 <- future126 %>% 
  mutate(year = substr(date,4,7)) %>% #crear fecha ano
  mutate(month = as.numeric(substr(date,1,2))) %>%
  filter(variable %in% c("pr", "tasmin","tasmax")) %>% 
  mutate(season = case_when(month >= 3  & month <= 5 ~ "spring")) %>%
  mutate(plotname = paste0(aoi_name,"_",as.character(year))) %>% #nombre de parcela+año
  select(plotname, variable, values, year,month,season,aoi_name) %>%
  pivot_wider(
    names_from = variable,
    values_from = values
  ) %>%
  # inner_join(aoi_names, by = "plotname") %>% #filtrar por parcelas de estudio
  left_join( aoi_coords, by = c("aoi_name" = "name")) %>%  #añadir latitud
  # CALCULAR PET y CMI
  mutate(
    ET0 = hargreaves(Tmin = tasmin, 
                     Tmax = tasmax, 
                     lat = unique(Y)[1],  # Assuming `Y` contains latitude
                     Pre = pr)
  ) %>%
  mutate(cmi = pr - ET0) %>%
  mutate(Date = as.Date(paste(year, month, 1, sep = "-"), format = "%Y-%m-%d")) %>%
  mutate(pr = round(pr,2),tasmin=round(tasmin,2),tasmax=round(tasmax,2)) %>%
  mutate(plotname = paste0(aoi_name,"_",as.character(year))) #nombre de parcela+año

#ordenar CMI
split_ssp126 <- split(ssp126, ssp126$aoi_name)

# Sort each dataframe by the 'date' column in ascending order
sorted_ssp126 <- lapply(split_ssp126, function(df) {
  df %>% arrange(Date)
})



process_spei <- function(df) {
  # #convertir cmi en time series
  cmi_ts <- ts(df$cmi, start = c(min(df$year), min(df$month)), frequency = 12)
  
  # Calculate SPEI for 3-month and 5-month scales
  spei3 <- spei(cmi_ts, scale = 3)$fitted # primavera
  spei5 <- spei(cmi_ts, scale = 5)$fitted # desde enero
  spei8 <- spei(cmi_ts, scale = 8)$fitted #desde otoño
  spei12 <- spei(cmi_ts, scale = 12)$fitted # 1 ano de ref
  spei24 <- spei(cmi_ts, scale = 24)$fitted # 2 ano
  cmi3 <- zoo::rollsumr(df$cmi, k = 3, fill = NA, align = "right")
  cmi5 <- zoo::rollsumr(df$cmi, k = 5, fill = NA, align = "right")
  
  # Create a new dataframe to store the SPEI values
  spei_df <- df %>%
    select(aoi_name, year, month, Date) %>%
    mutate(spei3 = spei3, spei5 = spei5,spei8 = spei8,spei12 = spei12,spei24 = spei24,
           cmi3 = cmi3, cmi5=cmi5)
  
  return(spei_df)
}

# Apply the function to each dataframe in the list
ssp126_spei_results <- lapply(sorted_ssp126, process_spei)
# el problema es que faltan datos para los años entre medias 

may_spei_ssp126 <- lapply(ssp126_spei_results, function(df) {
  df %>%
    filter(month == 5) %>%   # Filter for the month of May
    select(aoi_name, year, spei3, spei5,spei8,spei12,spei24,cmi3,cmi5)  # Select relevant columns
})

# Combine the results into a single dataframe if needed
var_spei_ssp126 <- do.call(rbind, may_spei_ssp126)
var_spei_ssp126 <- var_spei_ssp126 %>%
  mutate(plotname = paste0(aoi_name,'_',year))

write.csv(var_spei_ssp126, paste0(dir,'/2_RESULTS/var_spei_ssp126.csv'))





#######################################################
# SSP585
#######################################################

ssp585 <- future585 %>% 
  mutate(year = substr(date,4,7)) %>% #crear fecha ano
  mutate(month = as.numeric(substr(date,1,2))) %>%
  filter(variable %in% c("pr", "tasmin","tasmax")) %>% 
  mutate(season = case_when(month >= 3  & month <= 5 ~ "spring"))  %>%
  mutate(plotname = paste0(aoi_name,"_",as.character(year))) %>% #nombre de parcela+año
  select(plotname, variable, values, year,month,season,aoi_name) %>%
  pivot_wider(
    names_from = variable,
    values_from = values
  ) %>%
  # inner_join(aoi_names, by = "plotname") %>% #filtrar por parcelas de estudio
  left_join( aoi_coords, by = c("aoi_name" = "name")) %>%  #añadir latitud
  # CALCULAR PET y CMI
  mutate(
    ET0 = hargreaves(Tmin = tasmin, 
                     Tmax = tasmax, 
                     lat = unique(Y)[1],  # Assuming `Y` contains latitude
                     Pre = pr)
  ) %>%
  mutate(cmi = pr - ET0) %>%
  mutate(Date = as.Date(paste(year, month, 1, sep = "-"), format = "%Y-%m-%d")) %>%
  mutate(pr = round(pr,2),tasmin=round(tasmin,2),tasmax=round(tasmax,2)) %>%
  mutate(plotname = paste0(aoi_name,"_",as.character(year))) #nombre de parcela+año

#ordenar CMI
split_ssp585 <- split(ssp585, ssp585$aoi_name)

# Sort each dataframe by the 'date' column in ascending order
sorted_ssp585 <- lapply(split_ssp585, function(df) {
  df %>% arrange(Date)
})

process_spei <- function(df) {
  # #convertir cmi en time series
  cmi_ts <- ts(df$cmi, start = c(min(df$year), min(df$month)), frequency = 12)
  
  # Calculate SPEI for 3-month and 5-month scales
  spei3 <- spei(cmi_ts, scale = 3)$fitted # primavera
  spei5 <- spei(cmi_ts, scale = 5)$fitted # desde enero
  spei8 <- spei(cmi_ts, scale = 8)$fitted #desde otoño
  spei12 <- spei(cmi_ts, scale = 12)$fitted # 1 ano de ref
  spei24 <- spei(cmi_ts, scale = 24)$fitted # 2 ano
  cmi3 <- zoo::rollsumr(df$cmi, k = 3, fill = NA, align = "right")
  cmi5 <- zoo::rollsumr(df$cmi, k = 5, fill = NA, align = "right")
  
  # Create a new dataframe to store the SPEI values
  spei_df <- df %>%
    select(aoi_name, year, month, Date) %>%
    mutate(spei3 = spei3, spei5 = spei5,spei8 = spei8,spei12 = spei12,spei24 = spei24,
           cmi3 = cmi3, cmi5=cmi5)
  
  return(spei_df)
}

# Apply the function to each dataframe in the list
ssp585_spei_results <- lapply(sorted_ssp585, process_spei)
# el problema es que faltan datos para los años entre medias 

may_spei_ssp585 <- lapply(ssp585_spei_results, function(df) {
  df %>%
    filter(month == 5) %>%   # Filter for the month of May
    select(aoi_name, year, spei3, spei5,spei8,spei12,spei24,cmi3,cmi5) # Select relevant columns
})

# Combine the results into a single dataframe if needed
var_spei_ssp585 <- do.call(rbind, may_spei_ssp585)
var_spei_ssp585 <- var_spei_ssp585 %>%
  mutate(plotname = paste0(aoi_name,'_',year))

write.csv(var_spei_ssp585, paste0(dir,'/2_RESULTS/var_spei_ssp585.csv'))






