library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(terra)
library(sf)
library(SPEI)


dir <- 'C:/TFM/ScriptsR/3_varExp_otras'
setwd(dir)



# Load AOI and extract names
aoi <- vect("./1_DATA/aoi/coords_parcelas.shp")
aoi_names <- as.data.frame(aoi) %>%
# # separate_rows to split 'Periodo' by comma and create new rows
# separate_rows(Periodo, sep = ",") %>%
#   # renombramos como Aoi_name + año
# mutate(plotname = paste0(Name, "_", Periodo)) %>%
mutate(plotname_future = paste0(Name, "_", '2071')) 



