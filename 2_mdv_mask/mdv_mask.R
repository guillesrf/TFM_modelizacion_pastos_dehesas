library(terra)

# directorio
dir <- 'C:/TFM/ScriptsR/2_mdv_mask'
setwd(dir)

# Cargar directorios
files <- as.list(list.files(paste0(dir,'/1_DATA/MDV_PNOA/'), pattern="*.tif$", all.files=FALSE, full.names=TRUE))

# Stack
stack <- lapply(files,terra::rast) 
r1 <- rast(files[[1]])
savi <- rast(paste0(dir,'/1_DATA/savi.tif'))

#reproyectamos
stack_reproj <- lapply(stack,terra::project, y=r1) 

#Cambiar resolucion a 30x30m (LANDSAT)
stack_resample <- lapply(stack_reproj,terra::resample, y=savi)

#Mosaico
stack_mosaic <- do.call(terra::mosaic, stack_resample) # MUY LENTO, MEJOR EN QGIS
plot(stack_resample[[4]])
plot(stack_mosaic)
#Reclasificar a valores 1 
mosaic_30_reclas <- terra::classify(stack, c(0, 100, 0.5, 1),include.lowest=TRUE)
#Guardar a .tif unsigned int 8
writeRaster(mosaic_30_recla,paste0(dir,'/2_RESULTS/TREE_MASK/tree_mask.tif'),filetype="GTiff",datatype='INT2U', overwrite=TRUE)