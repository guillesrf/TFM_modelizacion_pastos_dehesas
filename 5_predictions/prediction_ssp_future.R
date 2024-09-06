library(terra)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(caret)
library(mgcv)
library(car)
dir <- 'C:/TFM/ScriptsR/4_dataframetomodel'
setwd(dir)



######################################################
#                                                    #
#  PREDICCION EN ESCENARIOS DE CC                    #
#                                                    #
######################################################

##############################################################################
# A. SIMULAR VALORES DE BANDAS
##############################################################################
dist_buffer = 500
# Valores de bandas e indices
# los archivos deben terminar en *_parcela_fecha.csv
files <- as.list(list.files(paste0(dir,'/1_DATA/csv_gee/buffer',dist_buffer), pattern='*.csv', all.files=FALSE, full.names=TRUE))
#Loop para sacar dfs en una lista
list_dfs <- list()
for (f in seq_along(files)) {
  # leer el df desde el csv
  df <- read.csv(files[[f]])
  list_dfs[[f]] <- df
}
# de varios df a uno
gee_df <- bind_rows(list_dfs)

# formateamos y limpiamos datos
gee_df <- gee_df %>% 
      mutate(date = as.Date(sub('^.*_', '',image_id), '%Y%m%d'), 
             year = as.character(year(date)),
             plotname = sub('^.*/', '',(paste0(str_remove(gee_df$aoi_name,'_point'),'_',year))),
             Farm = substr(plotname, 1, nchar(plotname)-5))

#gee_df <- na.omit(gee_df)


#VAMOS A ASUMIR EL VALOR PROMEDIO DE CADA BANDA Y PARCELA PARA SIMULAR LOS VALORES DE 2041-2070
simulated_spectral <- gee_df %>%
  group_by(Farm) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))



##############################################################################
# B. CARGAR OTRAS VAR. EXPLICATIVAS
##############################################################################

# ingesta de los dataframes

#Añadir valores de tn MS / ha con un join
yield_data <- read.csv(paste0(dir,'/1_DATA/yield/yield_data.csv'),header = TRUE, sep=';')
yield_data$plotname <- yield_data$Farm_code
#NOTA:  sin datos fuera del canopy ni sin fecha ni que no sea dehesa

#Climaticas
climate_ssp585 <- read.csv('C:/TFM/ScriptsR/3_varExp_otras/2_RESULTS/var_clim_ssp585.csv',header = TRUE, sep=',')


#Suelo
soil <- read.csv('C:/TFM/ScriptsR/3_varExp_otras/2_RESULTS/var_soil.csv',header = TRUE, sep=',')
soil <- soil %>%
  mutate(Farm = substr(plotname, 1, nchar(plotname)-5)) %>%
  group_by(Farm) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))


#Terreno
terrain <- read.csv('C:/TFM/ScriptsR/3_varExp_otras/2_RESULTS/terrain_stats_b500.csv',header = TRUE, sep=',')
terrain <- terrain %>%
  mutate(Farm = substr(plotname, 1, nchar(plotname)-5)) %>%
  group_by(Farm) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))


  #SPEI
#spei https://dds.cmcc.it/#/dataset/global-spei-ensemble-simulations/future_2040_2079?
# Load AOI and extract names
aoi <- vect("C:/TFM/ScriptsR/3_varExp_otras/1_DATA/aoi/coords_parcelas.shp")
aoi_names <- as.data.frame(aoi)
aoi_names <- aoi_names %>%
  mutate(ID = row_number())
# SPEI 12
spei_ssp585_raster <- rast('C:/TFM/ScriptsR/3_varExp_otras/1_DATA/spei_simulations/spei_global-spei-ensemble-simulations_future_2040_2079_H_GFDL_45_12_420105.nc')
spei_ssp585 <- terra::extract(spei_ssp585_raster,aoi)
spei_ssp585 <- spei_ssp585 %>%
  left_join(aoi_names, by = c('ID' = 'ID')) %>%
  mutate(spei12 = spei)



# JOIN de rendimientos, bandas y otras var exp.

keep_bands = c('yield_kgDM_ha.1',
                'tas_min_spring','RIPI','fcc','tas_min_year','pr_max_spring','bio15_year','pH_CaCl','spei12')
#               'Farm_code',
#               'BLUE','GREEN','RED','NIR','SWIR1','SWIR2','NDVI','NDTI','GNDVI','NDMI','NDWI','NGRDI','SAVI','BSI','EVI','AVI','MSI','GCI','OSAVI','MSAVI2','NLI','VARI','SOCI','RRECI','RVI',
#               'fcc',
#               'tas_max_year','tas_min_year','tas_mean_year', 'bio4_year','bio7_year', 'pr_max_year','pr_min_year','pr_mean_year', 'pr_accum_year' , 'bio15_year',
#               'tas_max_spring','tas_min_spring','tas_mean_spring', 'bio4_spring','bio7_spring', 'pr_max_spring','pr_min_spring','pr_mean_spring', 'pr_accum_spring' , 'bio15_spring',
#               "spei3","spei5","spei8","spei12","spei24",
#               'CEC','T_OC','CN','N','P','K','pH_CaCl','pH_H2O','DEPTH_ROOTS','T_BD','T_CLAY','T_SAND','T_SILT','T_GRAVEL','T_TAWC',
#               'slope','aspect'#,'TRI',#'roughness'
# )
#JUNTAMOS TODAS LAS VAR EXPL

# BANDAS E INDICES PROMEDIO EXTRAPOLADOS
data_ssp585 <- simulated_spectral %>%
  # SUELOS
  left_join(soil, by = c('Farm' = 'Farm')) %>%
  # TERRENO
  left_join(terrain, by = c('Farm' = 'Farm')) %>%
  # CLIMATICAS
  left_join(climate_ssp585, by = c('Farm' = 'aoi_name')) %>%
  # SPEI
  left_join(spei_ssp585, by = c('Farm' = 'Name')) %>%
  # na.omit() %>%
  mutate_if(is.integer, as.numeric)


# write data
write.csv(data_ssp585,paste0('C:/TFM/ScriptsR/5_predictions/2_RESULTS/data_ssp585_buffer',dist_buffer,'.csv'))



##############################################################################
# C. ELABORAR PREDICCIONES
##############################################################################

# Cargamos el modelo

gam1 <- readRDS("C:/TFM/ScriptsR/4_dataframetomodel/2_RESULTS/gam1_best_bf500.rds")
summary(gam1)
# performance_rmse(lm10)


#####
##   SSP585   

future_yield <- data_ssp585 %>%
  mutate(prediction_ssp585_gam1 = predict(gam1,data_ssp585)) %>%
  mutate(yield_kgDM_ha.1.x = prediction_ssp585_gam1 ) %>%
  mutate(Farm_code = paste0(Farm,'_2071')) %>%
  mutate(Year = '2071') %>%
  mutate(time_period = as.factor('F')) %>%
  select(all_of(c('Farm_code','yield_kgDM_ha.1.x','Year','prediction_ssp585_gam1','time_period')))
           
#future_yield$prediction_ssp585_gam1
#mean(future_yield$prediction_ssp585_gam1) #3913.62

####
##  Comparacion con el periodo de referencia    
####

data_historic <- read.csv('C:/TFM/ScriptsR/4_dataframetomodel/2_RESULTS/full_data_buffer500.csv')

historic_yield <- data_historic[,c('Farm_code','yield_kgDM_ha.1')] %>%
  mutate(prediction_ssp585_gam1 = predict(gam1,data_historic)) %>%
  left_join(yield_data, by=c('Farm_code' = 'plotname')) %>%
  mutate(time_period = as.factor('H')) %>%
  select(all_of(c('Farm_code','Year','yield_kgDM_ha.1.x','prediction_ssp585_gam1','time_period')))

yield_all <- rbind(historic_yield,future_yield) %>%
  mutate(plot = substr(Farm_code,1,nchar(Farm_code)-5))


# medias por periodo de referencia (H=historico, F=Futuro)
mean_time_period <- yield_all %>%
  group_by(time_period) %>%
  summarize(mean_yield = mean(yield_kgDM_ha.1.x, na.rm = TRUE))
mean_time_period

#normalidad


# medias por parcelas y periodo referencia
C3_C7_H <- data.frame(
  time_period = c('H', 'H'),
  plot = c('deheson_S3', 'deheson_S7'),
  mean_yield = c(4622.284, 4532.837)
)

mean_time_period_farm <- yield_all %>%
  group_by(time_period, plot) %>%
  summarize(mean_yield = mean(yield_kgDM_ha.1.x, na.rm = TRUE)) %>%
  ungroup() %>%
  rbind(C3_C7_H) %>%
  mutate(time_period = recode_factor(time_period, 'H' = '2002-2015', 'F' = '2071-2100')) %>%
  mutate(time_period = factor(time_period, levels = c('2002-2015', '2071-2100')))

## PLOT
ggplot(mean_time_period_farm, aes(x = plot, y = mean_yield, fill = time_period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(breaks = seq(0, max(mean_time_period_farm$mean_yield, na.rm = TRUE), by = 500)) +
  theme_minimal() +
  labs(
    title = "Rendimiento de biomasa de pasto herbáceo \npor explotación y período de referencia",
    x = " ",
    y = "kg DM/ha",
    fill = "Periodo de referencia"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",  
    legend.title = element_text(size = 12),  
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold") 
  )


#TESTS ESTADISTICOS
#MEDIAS ENTRE H Y F
# Subsetting data for H and F
H_data <- yield_all %>% filter(time_period == 'H') %>% pull(yield_kgDM_ha.1.x)
F_data <- yield_all %>% filter(time_period == 'F') %>% pull(yield_kgDM_ha.1.x)

# Normality Test for each group (Shapiro-Wilk Test)
shapiro_H <- shapiro.test(H_data)
shapiro_F <- shapiro.test(F_data)

# Checking the results of normality test
shapiro_H
shapiro_F

# Homogeneity of variances (Levene's test)

leveneTest(yield_kgDM_ha.1.x ~ time_period, data = yield_all) # varianza no homogénea


# TEST T
t_test <- t.test(H_data, F_data, var.equal = FALSE)
t_test # no podemso rechazar la hipotesis alternativa, i.e. el no hay diferencia 


