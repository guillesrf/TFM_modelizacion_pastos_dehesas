library(terra)
library(sf)
library(tidyr)
library(dplyr)

dir <- 'C:/TFM/ScriptsR/3_varExp_otras'
setwd(dir)

###########
# DATOS DE ENTRADA
###########

buff_dist = 500 # Buffer distance

# AOI
aoi <- vect('./1_DATA/aoi/coords_parcelas.shp')

# AOI buffer
aoi_buffer <- terra::buffer(aoi, buff_dist)
aoi_buffer <- project(aoi_buffer, 'EPSG:25830')

# MDE STACK
list_mde <- list.files('C:/TFM/Proyecto_QGIS/MDE', pattern='*.tif$', full.names = TRUE)
mde_stack <- lapply(list_mde, terra::rast)

# Reproject all rasters to the same CRS and resolution
mde_stack <- lapply(mde_stack, FUN=project, y='EPSG:25830')

# Merge all rasters into a single raster
merged_mde <- do.call(terra::mosaic, mde_stack)

# Mask the merged raster with the AOI buffer
#mde_masked <- mask(merged_mde, aoi_buffer) #no hace falta porque zonal coge el AOI

# Calculate terrain statistics (slope, aspect, TRI, roughness) for the merged and masked raster
mde_terrain <- terrain(merged_mde, v=c('slope','aspect','TRI','roughness'))

# Rasterize the AOI buffer using the masked raster as a template
#aoi_buffer_raster <- rasterize(aoi_buffer, merged_mde, field=1)

# Apply zonal statistics using the rasterized AOI buffer
terrain_stats <- zonal(mde_terrain, aoi_buffer, fun='mean',na.rm=TRUE)


## PASAR A DF

# Nombres de AOI
aoi_names <- as.data.frame(aoi)
aoi_names <- aoi_names %>%
  mutate(ID = row_number())


terrain_stats_save <- terrain_stats
# Replace AOI IDs with names, join with AOI, separate by years and rename 
terrain_stats <- terrain_stats %>%
  mutate(ID = row_number()) %>%
  left_join(aoi_names, by = c("ID" = "ID")) %>%
  # Use separate_rows to split 'Periodo' by comma and create new rows
  separate_rows(Periodo, sep = ",") %>%
  # Step 2: Create the new 'Name.x' values by appending the year
  mutate(plotname = paste0(Name, "_", Periodo))
terrain_stats <- terrain_stats[,-c(5:9)]
  


#####################
# guardamos los datos
####################


write.csv(terrain_stats,paste0(dir,'/2_RESULTS/terrain_stats_b',buff_dist,'.csv'))

