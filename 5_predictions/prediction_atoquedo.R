library(sf)
library(terra)
library(tidyterra)
library(dplyr)
library(lubridate)
library(fuzzyjoin)
library(mgcv) #GAM
library(e1071) # SVM
library(caret)
library(ggplot2)
library(purrr) 
library(RColorBrewer)
library(grid) # For drawing custom grobs

dir <- 'C:/TFM/ScriptsR/5_predictions'
setwd(dir)



######################################################
#                                                    #
#  1. INVERSION DEL MODELO EN ATOQUEDO #
#                                                    #  
######################################################

study_plot = 'atoquedo'
############################################################################
# A  PREDICCION RESPECTO A VALORES MEDIOS DE BANDAS E INDICES
############################################################################

# 1. cargar modelos

# lm2 <- readRDS("C:/TFM/ScriptsR/4_dataframetomodel/2_RESULTS/lm3_best_bf500.rds")
# summary(lm2)
# # performance_rmse(lm10)
# 
# glm1 <- readRDS("C:/TFM/ScriptsR/4_dataframetomodel/2_RESULTS/glm1_best_bf500.rds")
# summary(glm1)

gam1 <- readRDS("C:/TFM/ScriptsR/4_dataframetomodel/2_RESULTS/gam1_best_bf500.rds")
summary(gam1)

# svm2_test <- readRDS(file="C:/TFM/ScriptsR/4_dataframetomodel/2_RESULTS/svm2_bf500.rds")
# print(svm2_test)


#escojer bandas que usan los modelos
#names(best_model$model)
keep_vars = c('yield_kgDM_ha.1',
              'tas_min_spring','RIPI','fcc','tas_min_year','pr_max_spring','bio15_year','pH_CaCl','spei12',
              'RRECI')
#               'Farm_code',
#               'BLUE','GREEN','RED','NIR','SWIR1','SWIR2','NDVI','NDTI','GNDVI','NDMI','NDWI','NGRDI','SAVI','BSI','EVI','AVI','MSI','GCI','OSAVI','MSAVI2','NLI','VARI','SOCI','RRECI','RVI',
#               'fcc',
#               'tas_max_year','tas_min_year','tas_mean_year', 'bio4_year','bio7_year', 'pr_max_year','pr_min_year','pr_mean_year', 'pr_accum_year' , 'bio15_year',
#               'tas_max_spring','tas_min_spring','tas_mean_spring', 'bio4_spring','bio7_spring', 'pr_max_spring','pr_min_spring','pr_mean_spring', 'pr_accum_spring' , 'bio15_spring',
#               "spei3","spei5","spei8","spei12","spei24",
#               'CEC','T_OC','CN','N','P','K','pH_CaCl','pH_H2O','DEPTH_ROOTS','T_BD','T_CLAY','T_SAND','T_SILT','T_GRAVEL','T_TAWC',
#               'slope','aspect'#,'TRI',#'roughness'
# )



# 2. Preparacion de los datos

#Obtener bandas  para la secuencia 2002 - 2014 en cada pixel, desde GEE
# variable de respuesta
yield_data <- read.csv(paste0(dir,'/1_DATA/yield_data.csv'),header = TRUE, sep=';')
yield_data$plotname <- yield_data$Farm_code



# variable explicativa
# bandas e indices

spectral_df <- read.csv('./1_DATA/atoquedo/gee_data/atoquedo_completeSeries_L7_L8.csv',sep=';')
spectral_df$plotname <- spectral_df$aoi_name
spectral_df$Year <- substr(spectral_df$plotname, nchar(spectral_df$plotname)-3, nchar(spectral_df$plotname))



# climaticas
climate_df <- read.csv("C:/TFM/ScriptsR/3_varExp_otras/2_RESULTS/var_clim_historic.csv") #faltan los dos primeros años porque no había estacion meteo


# spei
spei_df <- read.csv("C:/TFM/ScriptsR/3_varExp_otras/2_RESULTS/var_spei_historic.csv")


# suelo
soil_df <- read.csv('./1_DATA/var_soil.csv')
soil_df$plot <- substr(soil_df$plotname, 1, nchar(soil_df$plotname) - 5)
soil_atoquedo_df <- soil_df %>% #resumimos para atoquedo
  filter(plot == study_plot) %>% 
  select(-X, -plotname) %>%  
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  mutate(plot = study_plot)

#stats del terreno
# terrain_df <- read.csv('terrain_stats_b500.csv')


#3. Predicciones

prediction_data <-  spectral_df %>%
  left_join(climate_df, c('plotname' = 'plotname')) %>%
  left_join(spei_df, c('plotname' = 'plotname')) %>%
  regex_left_join(soil_atoquedo_df, by = c('plotname' = 'plot')) %>%
  left_join(yield_data, c('plotname' = 'plotname')) #%>%
# select(all_of(keep_vars))

# Data log transformada para glm1
#prediction_data_log <- prediction_data
#prediction_data_log$yield_kgDM_ha.1 <- log(prediction_data_log$yield_kgDM_ha.1)

# prediction_data$predicted_lm2 <- predict(lm2, newdata = prediction_data) #hay valores negativos!
# prediction_data$predicted_glm1 <- exp(predict(glm1, newdata = prediction_data_log))
prediction_data$prediction_gam1 <- predict(gam1, newdata = prediction_data)

#4. Validacion de los errores



# #glm1
# 
# model_errors_glm1 <- prediction_data %>% 
#   filter(!is.na(yield_kgDM_ha.1)) %>% 
#   mutate(error_glm1 = yield_kgDM_ha.1-predicted_glm1)
# RMSE_glm1 <- sqrt(sum(model_errors_glm1$error_glm1^2)/nrow(model_errors_glm1))
# MAE_glm1 <- mean(abs(model_errors_glm1$error_glm1))
# stdev_glm1 <- mean(model_errors_glm1$error_glm1)
# 
# RMSE_glm1 #1431.69
# MAE_glm1 #1130.547



# gam1
model_errors_gam1 <- prediction_data %>% 
  filter(!is.na(yield_kgDM_ha.1)) %>% 
  mutate(error_gam1 = yield_kgDM_ha.1 - prediction_gam1)
RMSE_gam1 <- sqrt(sum(model_errors_gam1$error_gam1^2)/nrow(model_errors_gam1))
MAE_gam1 <- mean(abs(model_errors_gam1$error_gam1))
stdev_gam1 <- mean(model_errors_gam1$error_gam1)

RMSE_gam1 #646.9559
MAE_gam1 #536.8488

# PLOT

data_predict <- prediction_data 
data_predict <- prediction_data %>% 
  mutate(Prediction_gam1 = predict(gam1,data_predict)) %>%
  mutate(yield_kgDM_ha.1 = ifelse(is.na(yield_kgDM_ha.1), 0, yield_kgDM_ha.1)) %>%
  mutate(Year = substr(Farm_code,nchar(Farm_code)-3,nchar(Farm_code))) %>%
  mutate(Plot = substr(Farm_code,1,nchar(Farm_code)-5)) %>%
  select(all_of(c('Farm_code','Plot','Year','yield_kgDM_ha.1','prediction_gam1')))

data_predict$Year <- c(2004:2014)
data_predict$Plot <- 'Atoquedo'
data_predict$Farm_code <- paste0(data_predict$Plot,'_',data_predict$Year)

data_predict_long <- data_predict %>%
  pivot_longer(cols = c(yield_kgDM_ha.1, prediction_gam1), 
               names_to = "Type", 
               values_to = "Value") %>%
  mutate(Type = recode_factor(Type, 
                              "yield_kgDM_ha.1" = "Observados", 
                              "prediction_gam1" = "Predicción GAM1")) %>%
  mutate(Year = as.numeric(Year))  # Convert Year to numeric  #

# PLOT 1 : VALORES EN CADA A?O Y PARCELA


# Create the bar plot
ggplot(data_predict_long, aes(x = Year, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +  # Bar plot with bars side by side
  scale_x_continuous(breaks = 2004:2014) +  # Ensure x-axis has all years  labs(x = "Año", y = "kg MS / ha", fill = "Valor:") +  # Labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "bottom",
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5, lineheight = 1.2)) + 
  ggtitle('Serie reconstruida 2014-2024 de valores de biomasa de pasto \n observados vs estimados en la parcela Atoquedo')  



############################################################################
# B. PREDICCION POR PIXEL
############################################################################

######################
# 1. PREPARAR RASTERS


#AOI
# AOI
aoi <- vect('1_DATA/atoquedo/atoquedo_recinto_grande_25830.shp')
#aoi <- aoi[aoi$Name == study_plot, ] #si hubiera que filtrar

#cargar las imagenes procesadas de GEE
spectral_files <- list.files('./1_DATA/atoquedo/imagenes/reproyectado', pattern= '*.tif$', full.names=TRUE)

read_and_filter_raster <- function(file_path, keep_bands) {
  r <- rast(file_path)
  band_names <- names(r)
  selected_bands <- band_names[band_names %in% keep_bands]
  r_subset <- subset(r, selected_bands)
  return(r_subset)
}


#######################################################
# BANDAS E INDICES
# rasterizar y conservar bandas e indices de interes

spectral_stack <- lapply(spectral_files, read_and_filter_raster,
                         keep_bands = keep_vars)
spectral_reproj <- lapply(spectral_stack, FUN=project, y='EPSG:25830')
spectral_final <- lapply(spectral_reproj, FUN=crop, y=aoi, mask = TRUE,  touches = TRUE)
rm(spectral_stack, spectral_reproj)
#renobmrar RRECI a RIPI
spectral_final <- lapply(spectral_final, function(raster) {
  names(raster) <- "RIPI"
  return(raster)
})

##fcc
#extraemos la mascara de arboles binaria
tree_mask <- rast("./1_DATA/atoquedo/tree_mask/tree_mask_2c5.tif")
tree_mask[is.na(tree_mask)] <- 0 #eliminamos NA y los covnertimos en 0
# igualamos resolucion a raster de referencia
factor <- round(res(spectral_final[[1]])[1] / res(tree_mask)[1] , 0)
tree_mask_30 <- aggregate(tree_mask, fact = factor, fun = sum) #agrega, sumando el total de pixels
res(tree_mask_30)

# nº pixels
total_pixels <- factor^2 
# Percentage of pixels with value 1
fcc_raster <- (tree_mask_30 / total_pixels) * 100
fcc_raster <- crop(fcc_raster,aoi, mask=TRUE)
fcc_raster <- resample(fcc_raster, spectral_final[[1]], method = "bilinear")

#######################################################
# SUELO
aoi_buffer1000 <- buffer(aoi,1000)


pattern_soil <- paste0("(", paste(keep_vars, collapse = "|"), ").*\\.tif$")
soil_files <- list.files('C:/TFM/ScriptsR/3_varExp_otras/1_DATA/soil_reprojected/', pattern= pattern_soil, full.names=TRUE)
soil_stack <- lapply(soil_files,rast)
soil_reproj <- lapply(soil_stack, FUN=project, y='EPSG:25830')
soil_1000 <- lapply(soil_reproj, FUN=crop, y=aoi_buffer1000 , touches=TRUE)
soil_disagg <- lapply(soil_1000, FUN=disagg, fact= 17.42389 )
soil_cropped <- lapply(soil_disagg, FUN=crop, y=aoi, touches=TRUE)
soil_masked <- lapply(soil_disagg, FUN=crop, y=aoi,mask = TRUE,  touches = TRUE)
soil_final <- lapply(soil_masked, FUN=resample, y=spectral_final[[1]], threads=6)
rm(soil_stack,soil_reproj,soil_1000,soil_cropped,soil_disagg,soil_masked) 


#######################################################
# VAR CLIMA Y SPEI
#Extraer datos climaticos desde 2004-2014
climate_spei_df <- left_join(climate_df,spei_df, by= 'plotname')
clim_selected_vars <- names(climate_spei_df)[names(climate_spei_df) %in% keep_vars]
#filtramos por la parcela de estudio
climate_spei_plot <- climate_spei_df[climate_spei_df$aoi_name.x == study_plot, ]

list_clim_raster <- list()
#sacar las variables climaticas por año
for (i in unique(climate_spei_plot$Year)) {
  raster_name <- paste0('clim_spei_', i)
  # Filter data for the current year
  data <- climate_spei_plot[climate_spei_plot$Year == i, ]
  #desde raster vacio
  existing_raster <- spectral_final[[1]] 
  clim_raster <- rast(existing_raster)
  # Initialize an empty list to store each raster layer
  raster_layers <- list()
  # Loop through the selected variables and create corresponding raster layers
  for (var in clim_selected_vars) {
    new_raster <- rast(clim_raster) # Create a new SpatRaster with the same geometry
    values(new_raster) <- data[[var]] # Assign the values from the data frame
    cropped_raster <- crop(new_raster, y=aoi, mask=TRUE)
    raster_layers[[var]] <- cropped_raster # Store the raster in the list
  }
  # Combine all the created layers into a single SpatRaster
  clim_raster <- do.call(c, raster_layers)
  # Optionally, rename the layers (though they should already have the correct names)
  names(clim_raster) <- clim_selected_vars
  # Add the final SpatRaster to the list
  list_clim_raster[[raster_name]] <- clim_raster
}




rm(clim_raster)

#######################################################
# TERRAIN
# podemos ignorarlo visto el resultado del modelo



######################
# 2. MONTAR STACK
######################


study_years <- unique(climate_spei_plot$Year)


# nos aseguramso que esten incluidos los mismos años
years_clim <- unique(climate_spei_plot$Year)   # años con datos clima


# Convert list of SpatRaster objects to tibble, add years as identifiers, and subset
clim_subset <- tibble::tibble(value = list_clim_raster, year = years_clim) %>% 
  #eliminamos los años ausentes en spectral: 2000-2003 y 2015
  filter(year %in% years_clim[-c(1, 2, 3, 4, 16)]) %>%
  pull(value)



#funcion para crear el stack
it <- 1
stack_list <- list()
for (yr in years) {
  plotname <- paste0('stack_',study_plot,'_',yr)
  stack  <- c(spectral_final[[it]],
              soil_final[[1]],
              clim_subset[[it]]$tas_min_year,
              clim_subset[[it]]$tas_min_spring,
              clim_subset[[it]]$pr_max_spring,
              clim_subset[[it]]$bio15_year,
              clim_subset[[it]]$spei12,
              fcc_raster)
  names(stack) <- c("RIPI", "pH_CaCl","tas_min_year","tas_min_spring","pr_max_spring","bio15_year","spei12","fcc")
  stack_list[[plotname]] <- stack
  writeRaster(stack,
              paste0('C:/TFM/ScriptsR/5_predictions/2_RESULTS/stack_atoquedo/',plotname,'.tif'),
              filetype='GTiff',overwrite=TRUE)
  it <- it+1
  
}




#####
# 3. prediccion en cada raster
####

prediction_atoquedo <- lapply(stack_list, FUN= predict,model= gam1)
plot(prediction_atoquedo[[6]])


####
##4.  guardar resultados
####

for (i in names(prediction_atoquedo)) {
  name = i
  raster = prediction_atoquedo[[i]]
  writeRaster(raster, paste0(dir,'/2_RESULTS/prediccion_atoquedo/',name,'_predict.tif'),
              filetype='GTiff',overwrite=TRUE)
}

####
# 5. Representar
###
# unimos en un df 

# Convert the list of rasters into a tibble with a suitable format for ggplot
raster_tibble <- tibble(
  raster = prediction_atoquedo,
  id = seq_along(prediction_atoquedo)
)

# Extract data from each raster and combine into a single data frame
raster_df <- raster_tibble %>%
  mutate(raster = map(raster, ~ as.data.frame(.x, xy = TRUE))) %>%
  unnest(raster) %>%
  rename(x = x, y = y, value = lyr1) %>%
  mutate(id = factor(id, levels = 1:11, labels = years))

# aoi for plotting:
aoi_plot <- st_read('1_DATA/atoquedo/atoquedo_recinto_grande_25830.shp')
raster_crs <- crs(prediction_atoquedo[[1]])
aoi_plot <- st_transform(aoi_plot, crs(raster_crs))
aoi_df <- aoi_plot %>%
  st_transform(crs = crs(raster_crs)) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  group_by(L1) %>%
  summarize(x = list(X), y = list(Y), .groups = 'drop') %>%
  unnest(c(x, y))

# flecha del norte
north_arrow <- function() {
  grid::polygonGrob(
    x = unit(c(0.5, 0.45, 0.55, 0.5), "npc"),
    y = unit(c(0.05, 0.1, 0.1, 0.2), "npc"),
    gp = gpar(fill = "black", col = NA)
  )
}

## PLOT
# 
ggplot(raster_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  geom_polygon(data = aoi_df, aes(x = x, y = y), fill = NA, color = "black") + # Add polygon background
  facet_wrap(~ id, ncol = 3, labeller = as_labeller(setNames(years, as.character(1:11)))) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdYlGn")) + # Use ColorBrewer's Greens palette
  coord_fixed(ratio = 1) + # Ensures 1 unit of x = 1 unit of y
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add black border around each facet
    panel.spacing = unit(1, "lines"), # Increase spacing between facets
    strip.text = element_text(size = 10), # Adjust facet labels size
    axis.text = element_text(size = 5), # Adjust axis text size
    axis.text.x = element_text(angle = 90, vjust = 0.5), # Rotate x-axis labels
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), # Center and bold the title
    aspect.ratio = 1 # Ensure that the plot aspect ratio is square
  ) +
  labs(
    title = "Predicción de productividad de biomasa de pasto en primavera \nen la parcela Atoquedo entre 2004 y 2014",
    x = "Longitude",
    y = "Latitude",
    fill = "kg MS / ha" # Updated color scale label
  ) 
# +
#   annotation_custom(
#     grob = north_arrow(),
#     xmin = Inf, xmax = Inf, ymin = -Inf, ymax = Inf # Place the north arrow in the top right corner
#   )
