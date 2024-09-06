# librerias
library(dplyr)
library(tidyr)
library(caret) 
library(tidymodels)
library(olsrr)
library(fuzzySim)
library(janitor)
library(performance)
library(purrr)
library(car)
library(mgcv) #GAM
# library(mgcv.helper)
library(gamclass) #CV de GAM
library(gratia) #smoth vs response de un GAM
library(pdp) #partial dependance plots
library(pls) #PLSR
library(Cubist) #Cubist
library(e1071) # SVM
library(lime) #estimar importancia de variables en el SVM
library(rstanarm) #regresion bayesiana
library(glmnet) #lasso and ridge regression
library(flexmix) #calcular BIC y SBIC
library(pscl)
library(mgcViz)
library(DHARMa) #validar GLM
library(boot) #cross validation
library(DALEX) #varImp
library(stringr)
library(corrplot)
library(lubridate)
library(ggplot2)
library(ggcorrplot)
library(grid)



# directorio
dir <- 'C:/TFM/ScriptsR/4_dataframetomodel'
setwd(dir)

######################################################
#                                                    #
# A. PREPARACION DE LOS DATOS
#                                                    #
######################################################

dist_buffer = 500 #elegir el tamaño del buffer
# ingesta de los dataframes

#Añadir valores de tn MS / ha con un join
yield_data <- read.csv(paste0(dir,'/1_DATA/yield/yield_data.csv'),header = TRUE, sep=';')
yield_data$plotname <- yield_data$Farm_code
#NOTA:  sin datos fuera del canopy ni sin fecha ni que no sea dehesa

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
gee_df$date <- as.Date(sub('^.*_', '',gee_df$image_id), '%Y%m%d')
gee_df$year <- as.character(year(gee_df$date))
gee_df$plotname <- sub('^.*/', '',(paste0(str_remove(gee_df$aoi_name,'_point'),'_',gee_df$year)))
#gee_df <- na.omit(gee_df)

#Climaticas
climate <- read.csv('C:/TFM/ScriptsR/3_varExp_otras/2_RESULTS/var_clim_historic.csv',header = TRUE, sep=',')

#Suelo
soil <- read.csv('C:/TFM/ScriptsR/3_varExp_otras/2_RESULTS/var_soil.csv',header = TRUE, sep=',')

#Terreno
terrain <- read.csv('C:/TFM/ScriptsR/3_varExp_otras/2_RESULTS/terrain_stats_b500.csv',header = TRUE, sep=',')

#SPEI
spei <- read.csv('C:/TFM/ScriptsR/3_varExp_otras/2_RESULTS/var_spei_historic.csv')

# JOIN de rendimientos, bandas y otras var exp.

keep_bands = c('Farm_code','yield_kgDM_ha.1',
               'BLUE','GREEN','RED','NIR','SWIR1','SWIR2','NDVI','NDTI','GNDVI','NDMI','NDWI','NGRDI','SAVI','BSI','EVI','AVI','MSI','GCI','OSAVI','MSAVI2','NLI','VARI','SOCI','RIPI','RVI',
               'fcc',
               'tas_max_year','tas_min_year','tas_mean_year', 'bio4_year','bio7_year', 'pr_max_year','pr_min_year','pr_mean_year', 'pr_accum_year' , 'bio15_year',
               'tas_max_spring','tas_min_spring','tas_mean_spring', 'bio4_spring','bio7_spring', 'pr_max_spring','pr_min_spring','pr_mean_spring', 'pr_accum_spring' , 'bio15_spring',
               "spei3","spei5","spei8","spei12","spei24",
               'cmi3','cmi5',
               'CEC','T_OC','CN','N','P','K','pH_CaCl','pH_H2O','DEPTH_ROOTS','T_BD','T_CLAY','T_SAND','T_SILT','T_GRAVEL','T_TAWC',
               'slope','aspect'#,'TRI',#'roughness'
               )

# BANDAS E INDICES
data <- yield_data %>%
  left_join(gee_df, by = c('plotname' = 'plotname')) %>%
# CLIMATICAS
  left_join(climate, by = c('plotname' = 'plotname')) %>%
# SUELOS
  left_join(soil, by = c('Farm_code' = 'plotname')) %>%
# TERRENO
  left_join(terrain, by = c('Farm_code' = 'plotname')) %>%
# SPEI
  left_join(spei, by = c('Farm_code' = 'plotname')) %>%
  na.omit() %>%
  select(all_of(keep_bands)) %>%
  mutate_if(is.integer, as.numeric)

 
#remove_constant(yield_varExp)
write.csv(data,paste0(dir,'/2_RESULTS/full_data_buffer',dist_buffer,'.csv'))

#rm(climate,df,files,gee_df,list_dfs,soil,terrain,yield_data)


# B. OBTENCION DE MODELOS

X_data <- data[,-c(1,2)]
Y_data <- data$yield_kgDM_ha.1

X_data_normalized <- as.matrix(scale(data[, -c(1,2)])) #normalizado
Y_data_normalized <- scale(data$yield_kgDM_ha.1)


# Create a split (75% training, 25% testing)
split <- initial_split(data, prop = 0.75)

# Extract training and testing sets
train_data <- training(split)
test_data <- testing(split)

Y_train <- train_data$yield_kgDM_ha.1
X_train <- train_data[, -c(1,2)]

Y_test<- test_data$yield_kgDM_ha.1
X_test<- test_data[, -c(1,2)]

varDep <- 'yield_kgDM_ha.1'
num_vars = length(data)

explained_variance_score <- function(true_values, predictions) {
  var_residual <- var(true_values - predictions)
  var_true <- var(true_values)
  explained_variance <- 1 - var_residual / var_true
  return(explained_variance)
}

calculate_bic <- function(model) {
  final_model <- model$finalModel
  predicted <- predict(final_model, ncomp = model$bestTune$ncomp)
  residuals <- model$trainingData$.outcome - predicted
  n <- nrow(model$trainingData)
  k <- model$bestTune$ncomp
  rss <- sum(residuals^2)
  log_likelihood <- -n/2 * (log(2 * pi) + 1 - log(n) + log(rss))
  bic <- log(n) * k - 2 * log_likelihood
  
  return(bic)
}

calculate_bic_predict <- function(model) {
  final_model <- model$finalModel
  predicted <- predict(final_model, newdata = model$trainingData)
  residuals <- model$trainingData$.outcome - predicted
  n <- nrow(model$trainingData)
  k <- length(coef(final_model))  # Approximate number of parameters
  rss <- sum(residuals^2)
  log_likelihood <- -n/2 * (log(2 * pi) + 1 - log(n) + log(rss))
  bic <- log(n) * k - 2 * log_likelihood
  
  return(bic)
}
# Function to calculate MAE for Linear Regression
calculate_mae_lm <- function(model, data) {
  # Predict on the data
  predicted <- predict(model, newdata = data)
  
  # Compute residuals and MAE
  residuals <- abs(data[[all.vars(model$call)[1]]] - predicted)
  mae <- mean(residuals)
  
  return(mae)
}

# Function to calculate MAE for GLM
calculate_mae_glm <- function(model, data) {
  # Predict on the data
  predicted <- predict(model, newdata = data, type = "response")
  
  # Compute residuals and MAE
  residuals <- abs(data[[all.vars(model$call)[1]]] - predicted)
  mae <- mean(residuals)
  
  return(mae)
}

# Function to calculate MAE for GAM
calculate_mae_gam <- function(model, data) {
  # Predict on the data
  predicted <- predict(model, newdata = data)
  
  # Compute residuals and MAE
  residuals <- abs(data[[all.vars(model$call)[1]]] - predicted)
  mae <- mean(residuals)
  
  return(mae)
}

######################################################
#                                                    #
# 1. MODELOS LINEALES
#                                                    #
######################################################

# 1.1 Selección de variables basados en criterios de colinearidad con el 'paquete fuzzySim'
# plot del correlograma
corr<- cor(data[-c(1,2)])
#corrplot(corr,method='circle')

# 1.2 seleccion de variables on correlacion < 0.7
# cor_sel_pearson <- corSelect(data, sp.cols = 2, var.cols = 3:58, select='cor', cor.thresh = 0.7)
# print(cor_sel_pearson$selected.vars)

cor_sel_spearman <- corSelect(data, sp.cols = 2, var.cols = 3:58, select='cor', cor.thresh = 0.8,method='spearman')
print(cor_sel_spearman$selected.vars)

data_noCor <- data %>% select(all_of(c(varDep,cor_sel_spearman$selected.vars,'pH_CaCl','P','fcc'))) %>% select(-all_of(c('CEC','GNDVI')))

corr_good <- cor(data_noCor[,-1])


ggcorrplot(corr_good, method = "circle", type = "upper", 
           lab = TRUE, lab_size = 3, 
           colors = c("blue", "white", "red"),
           title = "Correlation Matrix",
           ggtheme = theme_minimal())




# 1.3 Seleccion por pasos
# seleccion de variables stepwise con el paquete 'olsrr'

lm0 <- lm(paste0(varDep,'~.'),data=data_noCor)
print(lm0) 
summary(lm0)

lm_sr_both_005  <- ols_step_both_p(lm0, p_ent=0.05, p_rem=0.05)
print(lm_sr_both_005)

lm_sr_both_005$model$model[0,]

lm_sr_both_05  <- ols_step_both_p(lm0, p_ent=0.5, p_rem=0.5)
print(lm_sr_both_05)

# 1.4 vemos  las combinaciones posibles entre una seleccion 
good_vars <- ' RIPI +  CEC + T_OC + fcc + tas_min_spring + pr_min_year + pr_min_year + bio15_year + bio15_spring+bio7_spring+spei12 '
lm_subset <- lm(paste0(varDep,'~ ', good_vars),data=data[,-1])


#1.5 mejor subset,seleccionado por pasos desde las variables no correlacionadas
# lm_bestsubset <- ols_step_best_subset(lm_subset, include ='fcc')
#                   #metric = c("rsquare", "adjr", "predrsq", "cp", "aic", "sbic", "sbc", 'fpe'))
# LO HE CALCULADO EN OTRO SCRIPT PARALELIZANDO 
# print(lm_bestsubset)
lm_bestsubset <- readRDS('./2_RESULTS/best_subsets.rds')

#EVALUACION DE LOS MODELOS

#LM1
lm1 <- lm(yield_kgDM_ha.1 ~RIPI +  CEC + T_OC + fcc + tas_min_spring + pr_min_year + pr_min_year + bio15_year + bio15_spring+bio7_spring+spei12 ,data=data[,-1])
summary(lm1)  # Multiple R-squared:  0.7348,	Adjusted R-squared:  0.6142  ,
#RMSE
RMSE_lm1 = sqrt(mean(lm1$residuals^2))
RMSE_lm1 #1114.049
# BIC
BIC(lm1) # 598.6479
# Comprobamos VIF y homocedasticidad
vif(lm1) #bien
check_heteroscedasticity(lm1) #Bien
ols_test_normality(lm1) # No es normal


# MODELO 2
#mejor modelo
lm2 <- lm(yield_kgDM_ha.1 ~ RIPI+ fcc+ tas_min_spring+ bio15_year+pH_CaCl, data=data)
summary(lm2)  # R2= 0.8822, RMSE
# El modelo 2 es mas sencillo presenta buen R2 = 0.8822 pero con muchas variables NO significativas
# Comprobamos VIF y homocedasticidad
summary(lm2)  #Multiple R-squared:  0.7539,	Adjusted R-squared:  0.7084  
RMSE_lm2 = sqrt(mean(lm2$residuals^2))
RMSE_lm2 #1073.067
BIC(lm2) # 578.6917
vif(lm2) #BIEN
check_heteroscedasticity(lm2) #normal
ols_test_normality(lm2) #homocedastico

## Validacion cruzada

#Set the seed to ensure the results are repeatable
set.seed(1984)
# Hyperparameters de los modelos lineales
lm_control <- trainControl(method = 'cv', #Cross Validation
                           number = 10, #number of groups
                           savePredictions = TRUE)

X_data_lm2 <- data[,c('tas_min_spring','RIPI','fcc','tas_min_year','pr_max_spring','bio15_spring','pH_CaCl')]

lm2_cv <- train(
  x = X_data_lm2,
  y = Y_data ,
  method = "lm",
  trControl = lm_control
)
#ver el modelo
summary(lm2_cv) 
print(lm2_cv)  
# RMSE      Rsquared   MAE     
# 1163.305  0.8503049  958.0853



# LM3

lm3 <- lm(yield_kgDM_ha.1 ~ tas_min_spring + RIPI +fcc +tas_min_year + pH_CaCl , data=data)
summary(lm3) #Multiple R-squared:  0.7701,	Adjusted R-squared:  0.7275  
RMSE_lm3 = sqrt(mean(lm3$residuals^2))
RMSE_lm3 #1037.245
calculate_mae_lm(lm3,data) #835.3233
BIC(lm3) # 576.4508
vif(lm3) # MAL
check_heteroscedasticity(lm3) # BIEN
ols_test_normality(lm3) # BIEN



# LM4 
data_lm4 <- as.data.frame(scale(data[,-1], center = TRUE, scale = TRUE))

lm4 <- lm(yield_kgDM_ha.1 ~ RIPI+ fcc+ tas_min_spring+tas_min_spring+ bio15_year+pH_CaCl, data = data_lm4)
summary(lm4) #Multiple R-squared:  0.7223,	Adjusted R-squared:  0.6709 , no mejora el modelo lineal
RMSE_lm4 = sqrt(mean(lm4$residuals^2))
RMSE_lm4 #0.47308
BIC(lm4) # 574.1025
vif(lm4) # bien
check_heteroscedasticity(lm4) # MAL
ols_test_normality(lm4) # BIEN






# MEJOR MODELO : lm2

saveRDS(lm2,'./2_RESULTS/lm2_best_bf500.rds')


######################################################
#                                                    #
#  2.  GLM
#                                                    #
######################################################

###
#glm1 , log link
###
glm1 <- glm(yield_kgDM_ha.1 ~RIPI+ fcc+ tas_min_spring+tas_min_spring+ bio15_year+pH_CaCl,
            family=gaussian(link ='log'), data = data)
summary(glm1)
R2_glm1 <-with(summary(glm1), 1 - deviance/null.deviance)
R2_glm1 #R2=0.7462417 #similar a lm3
vif(glm1) #BIEN, maxVIF 3.45
RMSE_glm1 = sqrt(mean(glm1$residuals^2))
RMSE_glm1 #  (log) 0.43932
calculate_mae_glm(glm1,data) #878.3877
BIC(glm1) # 579.707

#validacion cruzada
set.seed(1984)
X_data_glm1 <- data[,c('RIPI', 'fcc','tas_min_spring','tas_min_spring','bio15_year','pH_CaCl')]

glm1_cv <- train(
  x = X_data_glm1 ,
  y = Y_data ,
  method = "glm",
  family = gaussian(link = "log"),
  trControl = lm_control#,
)
summary(glm1)
performance_rmse(glm1) #1089.701

predictions_glm1 <- predict(glm1, data)
evs_glm1 <- explained_variance_score(Y_data, predictions_glm1)
print(paste("Explained Variance Score:", round(evs_glm1, 4))) #"Explained Variance Score: 4e-04"


#glm2, gamma link

#solo variables positivas
positiveVars<- data[-c(1,2)] %>% select_if(~ all(. >= 0))
cor_sel_spearman_positive <- corSelect(data, sp.cols = 2, var.cols = 3:63, select='cor', cor.thresh = 0.8,method='spearman')
print(cor_sel_spearman_positive$selected.vars)
data_noCor_positive <- data %>% select(all_of(c(varDep,cor_sel_spearman_positive$selected.vars)))

#GLM 2
glm2 <- glm(yield_kgDM_ha.1 ~ RIPI+ fcc+ tas_min_spring+tas_min_spring+ bio15_year+pH_CaCl, data=data[,-1], family = Gamma)
summary(glm2) # no mejora respecto al lineal
#R2
R2_glm2 <-with(summary(glm2), 1 - deviance/null.deviance)
R2_glm2 #R2= 0.4774002



#guardar el mejor modelo, tambien vale GLM6 

saveRDS(glm1,'./2_RESULTS/glm1_best_bf500.rds')


######################################################
#                                                    #
#  3.  GAM                                           #
#                                                    #
######################################################

# Calculamos el numero de valores unicos
unique_counts <- sapply(data[,-1], function(x) length(unique(x)))
unique_counts

#GAM1
#good_vars <- ' RIPI +  CEC + T_OC + fcc + tas_min_spring + pr_min_year + pr_min_year + bio15_year + bio15_spring+bio7_spring+spei12 '
gam1 <- gam(yield_kgDM_ha.1 ~ s(tas_min_spring, k=11) + s(RIPI) + s(spei12)  + s(pH_CaCl,fcc, k=10)+s(bio15_year, k=10), data = data[,-1],fit=TRUE)
summary(gam1) #R-sq.(adj) =  0.923   Deviance explained = 97.1%
formula(gam1)

# validacion
gam.check(gam1)
plot(gam1,residuals=TRUE)
RMSE_gam1 = sqrt(mean(gam1$residuals^2))
RMSE_gam1 #mejor resultado testado =367.7199
calculate_mae_gam(gam1,data) #307.5386
BIC(gam1) # 560.264
check_heteroscedasticity(gam1) # BIEN
vif(gam1)
mgcv::concurvity(gam1,full=FALSE)

# regam1# relaciones entre variables y var_resp
par(mfrow = c(2, 1))
plot.gam(gam1, pages = 1) #veamos las relaciones #practicamente lineal excepto tas_min_year

rsd <- residuals(gam1)
qq.gam(gam1,rep=100); plot(fitted(gam1),rsd)
plot(data$yield_kgDM_ha.1,rsd); plot(data$RIPI,rsd)



# TEST OVERFITTING
set.seed(1984) # For reproducibility
data_noID<- data[,-1]
train_indices <- sample(seq_len(nrow(data_noID)), size = 0.7 * nrow(data_noID))
train_data <- data_noID[train_indices, ]
test_data <- data_noID[-train_indices, ]
predictions_gam1 <- predict(gam1, newdata = test_data)
test_rmse_gam1 <- sqrt(mean((test_data$yield_kgDM_ha.1 - predictions_gam1)^2))
print(test_rmse_gam1) #RMSE 422.7087, parecido; aceptable


#### Cross-Validation
#con gamclass
gam1_cv <- CVgam(yield_kgDM_ha.1 ~ s(tas_min_spring,k=k=11) + s(RIPI) + s(spei12) + s(pH_CaCl,fcc,k=11) + s(bio15_year,k=10),
                 data = data, method = "REML")

#con caret
folds <- createFolds(data$yield_kgDM_ha.1, k = 10, list = TRUE, returnTrain = TRUE)
rmse_values <- numeric(length(folds))
# CV
# Loop through each fold
for(i in seq_along(folds)) {
  # Split the data into training and validation sets
  train_indices <- folds[[i]]
  train_data <- data[train_indices, ]
  validation_data <- data[-train_indices, ]
  
  # Fit the GAM on the training data
  gam_model <- gam(yield_kgDM_ha.1 ~ s(tas_min_spring) + s(RIPI) + s(spei12) + 
                     s(pH_CaCl) + s(fcc) + s(bio15_year),
                   data = train_data, method = "REML")
  
  # Predict on the validation set
  predictions <- predict(gam_model, newdata = validation_data)
  
  # Calculate RMSE for this fold
  actuals <- validation_data$yield_kgDM_ha.1
  rmse_values[i] <- sqrt(mean((predictions - actuals)^2))
}

# Calculate the mean and standard deviation of the RMSE across all folds
mean_rmse <- mean(rmse_values)
sd_rmse <- sd(rmse_values)

cat("Mean RMSE across folds:", mean_rmse, "\n") #1337.367
cat("Standard deviation of RMSE:", sd_rmse, "\n") #1006.057 



#LOOCV
n <- nrow(data)
rmse_values <- numeric(n)

for(i in 1:n) {
  # Split the data into training and validation sets
  train_data <- data[-i, ]
  validation_data <- data[i, , drop = FALSE]
  
  # Fit the GAM on the training data
  gam_model <- gam(yield_kgDM_ha.1 ~ s(tas_min_spring) + s(RIPI) + s(spei12) + 
                     s(pH_CaCl) + s(fcc) + s(bio15_year),
                   data = train_data, method = "REML")
  
  # Predict on the validation set
  prediction <- predict(gam_model, newdata = validation_data)
  
  # Calculate RMSE for this fold
  actual <- validation_data$yield_kgDM_ha.1
  rmse_values[i] <- sqrt(mean((prediction - actual)^2))
}

# Calculate the mean RMSE across all folds
mean_rmse <- mean(rmse_values)
cat("Mean RMSE from LOO-CV:", mean_rmse, "\n") #1370.532


#
saveRDS(gam1,'./2_RESULTS/gam1_best_bf500.rds')

######################################################
#                                                    #
#  4.  REGRESION MINIMOS CUADRADOS PARCIALES (PLSR)  #
#                                                    #
######################################################

X_data <- data[, -c(1,2)] #sin normalizar
Y_data <- data$yield_kgDM_ha.1

set.seed(1984)  # For reproducibility
cv_control <- trainControl(method = "cv", number = 10)

# PLSR 1
plsr_model1 <- train(
  x = X_data,            # Predictor variables
  y = Y_data,            # Response variable
  method = "pls",        # Method for Partial Least Squares Regression
  trControl = cv_control, # Cross-validation control
  tuneLength = 10        # Number of components to consider (can be adjusted)
)

# View the results
print(plsr_model1) 
# ncomp   RMSE      Rsquared   MAE  
# 2     2211.957  0.5219802  1787.983

bic_plsr1 <- calculate_bic(plsr_model1)
bic_plsr1



######################################################
#                                                    #
#  5.  CUBIST                                        #
#                                                    #
######################################################

set.seed(1984)

train_control_cubist <- trainControl(method = "CV", number = 10)
# train_control_cubist <- trainControl(method = "LOOCV", p = 0.3)
# intento 1
cubist_model1 <- train(
  yield_kgDM_ha.1 ~ .,
  data = data[,-1],
  method = 'cubist',
  preProcess = c("center", "scale"),
  trControl = train_control_cubist 
)

#summary(cubist_model1)
print(cubist_model1) 

# committees  neighbors  RMSE      Rsquared   MAE
# 20          0          1786.665  0.6515113  1491.825
bic_cubist1 <- calculate_bic_predict( cubist_model1)
bic_cubist1


######################################################
#                                                    #
#  6.  SVM                                           #
#                                                    #
######################################################


set.seed(1984)
svm_control <- trainControl(method = 'LOOCV', 
                            number = 10,  
                            search = 'grid', 
                            savePredictions = TRUE)

svm_grid <- expand.grid(
  C = c(0.001, 0.01, 0.1, 1, 10, 100), # Regularization parameter
  sigma = c(0.001, 0.01, 0.1,1,10,100)  # Kernel width
)

## svm 1
# train the model
svm1 <- train(
  x = X_data,
  y = Y_data,
  method = "svmRadial",
  preProcess = c("center","scale"),
  trControl = svm_control,
  tuneGrid = svm_grid
) 
saveRDS(svm1,paste0('./2_RESULTS/svm1_bf',dist_buffer,'.rds'))
# svm1 <- readRDS('./2_RESULTS/svm1_bf500.rds')

print(svm1$finalModel)
# C      sigma  RMSE      Rsquared   MAE 
# 1e-02  1e+00  2252.563  0.7782423  1951.497

# Mejores parametros
best_params_svm1 <- svm1$bestTune
print(best_params_svm1)


# importancia de las variables
importance_svm1 <- varImp(svm1, scale = FALSE)
print(importance_svm1)
plot(importance_svm1)

bic_svm1 <- calculate_bic(svm1)
bic_svm1


## svm 2
# sin normalizacion de datos
svm2 <- train(
  x = X_data,
  y = Y_data,
  method = "svmRadial",
  trControl = svm_control,
  tuneGrid = svm_grid,
) 
saveRDS(svm2,file="./2_RESULTS/svm2_bf500.rds")

print(svm2)
# C      sigma  RMSE      Rsquared   MAE 
# 1e-02  1e+01  1796.520  0.332241465  1499.613
#

# Mejores parametros
best_params_svm2 <- svm2$bestTune
print(best_params_svm2)


# importancia de las variables
importance_svm2 <- varImp(svm2, scale = FALSE)
print(importance_svm2)
plot(importance_svm2)


######################################################
#                                                    #
#  7.  NEURAL NETWORK             #
#                                                    #
######################################################


set.seed(1984)
nnet_control <- trainControl(method = 'LOOCV', 
                            number = 10, 
                            search = 'grid', 
                            savePredictions = TRUE)

nnet_grid <- expand.grid(size = c(1, 3, 5),   # Number of hidden units
                         decay = c(0.1, 0.01, 0.001)) 


## nnet 1
# train the model
nnet1 <- train(
  x=X_data,
  y=Y_data,
  method = "nnet",
  preProcess = c("center","scale"),
  trControl = nnet_control,
  tuneGrid = nnet_grid,
  linout = TRUE)  # linout =  TRUE para regresion

saveRDS(nnet1,paste0('./2_RESULTS/nnet1_bf',dist_buffer,'.rds'))
#nnet1 <- readRDS('./2_RESULTS/nnet1_bf500.rds')
print(nnet1)
# size  decay   RMSE      Rsquared    MAE
#       0.100   1902.539  0.26641739  1687.692

bic_nnet1 <- calculate_bic_predict(nnet1)
bic_nnet1


######################################################
#                                                    #
#  8.  REGRESION BAYESIANA                           #
#                                                    #
######################################################
# 
# # data_s <- as.data.frame(data[,-1])
# data_s <- as.data.frame(scale(data[,-1]))
# 
# # Fit a Bayesian linear regression model
# bayesian_model <- stan_glm(yield_kgDM_ha.1 ~ ., data = data_s, family = gaussian())
# summary(bayesian_model)
# # saveRDS(bayesian_model,'./2_RESULTS/bayesian_bf500')

# 4: The largest R-hat is 1.09, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat 
# 5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# r2_bayes <- bayes_R2(bayesian_model)  


######################################################
#                                                    #
#  8.  RIDGE Y LASSO (regularizacion)                #
#                                                    #
######################################################

##funcion para bic



# Ajustar Ridge Regression (alpha = 0), Lasso (alpha = 1), or Elastic Net (0 < alpha < 1)
# RIDGE
ridge_model <- cv.glmnet(as.matrix(X_data), Y_data, alpha = 0)
print(ridge_model)
R2_ridge <- max(1-ridge_model$cvm/var(Y_data)) #R2
R2_ridge #0.2095795
plot(ridge_model) #RME y RMSE
predictions_ridge <- predict(ridge_model, s = ridge_model$lambda.min, newx = as.matrix(X_data))
rmse_ridge <- sqrt(mean((Y_data - predictions_ridge)^2))
rmse_ridge #1556.443 unscaled



#ELASTIC NET
elastic_net_model <- cv.glmnet(as.matrix(X_data), Y_data, alpha = 0.5)
print(elastic_net_model)
R2_elastic <- max(1-elastic_net_model$cvm/var(Y_data)) #R2
R2_elastic # 0.4040498
plot(elastic_net_model)  #RME y RMSE
predictions_elastic_net <- predict(elastic_net_model, s = elastic_net_model$lambda.min, newx = as.matrix(X_data))
rmse_elastic_net <- sqrt(mean((Y_data - predictions_elastic_net)^2))
rmse_elastic_net #1212.93 CV unscaled


# LASSO 1
lasso_model <- cv.glmnet(as.matrix(X_data), Y_data, alpha = 1)
print(lasso_model)
R2_lasso <- max(1-lasso_model$cvm/var(Y_data)) #R2
R2_lasso # 0.3705848
plot(lasso_model)  #RME y RMSE
predictions_lasso <- predict(lasso_model, s = lasso_model$lambda.min, newx = as.matrix(X_data))
rmse_lasso <- sqrt(mean((Y_data - predictions_lasso)^2))
rmse_lasso # 1169.625

lasso_bic <- calculate_bic_predict(lasso_model)

  
######################################################
#                                                    #
#  9.  K-NEAREST NEIGHBOURS                          #
#                                                    #
######################################################
# set.seed(1984)
# train_control_knn <- trainControl(method = "LOOCV", number = 10)
# 
# 
# knn_model <- train(yield_kgDM_ha.1 ~ ., data = data[,-1],
#                    method = "knn",
#                    tuneGrid   = expand.grid(k = 1:10),
#                    #metric     = "RMSE", 
#                    preProcess = c("center", "scale"),
#                    tuneLength = 20,
#                    trControl = train_control_knn )
# print(knn_model) 
# #k   RMSE      Rsquared   MAE
# # 6  1857.312  0.28521542  1549.542




######################################################
#                                                    #
#  10.COMPARAR MODELOS                        #
#                                                    #
######################################################

#todos los modelos
# all_models <- list(lm1,lm2,lm3,lm4,lm6,lm7,lm8,lm9,lm10,
#                     glm1,glm2,glm3,glm4,glm5,
#                     gam_model1,gam_model2,gam_model3,gam_model4,gam_model5,gam_model6,
#                     plsr_model1,plsr_model2,plsr_model3,plsr_model4,
#                     cubist_model1,cubist_model2,cubist_model3,
#                     ridge_model,elastic_net_model,lasso_model,
#                     svm_model1,bayesian_model,knn_model)
# 
# all_model_compare <- lapply(best_models, performance::model_performance)
# all_model_compare[1]
# print(all_model_compare)


#mejores modelos
best_models <- list(lm3, glm1, gam1)
best_model_compare <- lapply(best_models, performance::model_performance)
print(best_model_compare)
#lm1 el mejor

######################################################
#                                                    #
#  11.GRAFICAS Y FIGURAS                      #
#                                                    #
######################################################
#GRAFICAS DE VALIDACION


# Call the function with your GAM model


####
# lm2
####

# #validacion
# # distribucion residuales
# sim_res_lm2 <- simulateResiduals(lm2)
# plot(sim_res_lm2)
# 
# #Q-Q
# qqnorm(residuals(lm2, type = "pearson"))
# qqline(residuals(lm2, type = "pearson"), col = "red")
# 
# # incluence y leverage
# cooks.distance <- cooks.distance(lm2)
# plot(cooks.distance, ylab = "Cook's Distance", main = "Cook's Distance")
# abline(h = 4 / length(cooks.distance), col = "red")
# 
# #Deviance residuals
# ypred_lm2 = predict(lm2)
# res_lm2 = residuals(lm2, type = 'deviance')
# plot(ypred_lm2,res_lm2)
# hist(res_lm2)
# 
# #Deviance Goodness-of-fit
# # deviance_lm2 = lm2$deviance
# # p.value_lm2 = pchisq(deviance_lm2, df = lm2$df.residual, lower.tail = F)
# # p.value_lm2 # 0.99, fail to reject the null hypothesis that the model fits the data well.
# 
# #RESIDUALES VS FITTED
# plot(fitted(lm2), residuals(lm2, type = "pearson"),
#      xlab = "Fitted Values",
#      ylab = "Pearson Residuals",
#      main = "Residuals vs Fitted Values")
# abline(h = 0, col = "red")
# 
# hist(residuals(lm2, type = "pearson"),
#      main = "Histogram of Pearson Residuals",
#      xlab = "Residuals",
#      breaks = 20)
# 
# ####
# #glm1
# ####
# 
# #validacion
# # distribucion residuales
# sim_res_glm1 <- simulateResiduals(glm1)
# plot(sim_res_glm1)
# 
# #Q-Q
# qqnorm(residuals(glm1, type = "pearson"))
# qqline(residuals(glm1, type = "pearson"), col = "red")
# 
# 
# # incluence y leverage
# cooks.distance <- cooks.distance(glm1)
# plot(cooks.distance, ylab = "Cook's Distance", main = "Cook's Distance")
# abline(h = 4 / length(cooks.distance), col = "red")
# 
# #dispersion
# dispersion <- sum(residuals(glm1, type = "pearson")^2) / glm1$df.residual
# dispersion #0.08538754
# #Deviance residuals
# ypred_glm1 = predict(glm1)
# res_glm1 = residuals(glm1, type = 'deviance')
# plot(ypred_glm1,res_glm1)
# hist(res_glm1)
# 
# #Deviance Goodness-of-fit
# deviance_glm1 = glm1$deviance
# p.value_glm1 = pchisq(deviance_glm1, df = glm1$df.residual, lower.tail = F)
# p.value_glm1 # 0.99, fail to reject the null hypothesis that the model fits the data well.
# 
# #RESIDUALES VS FITTED
# plot(fitted(glm1), residuals(glm1, type = "pearson"),
#      xlab = "Fitted Values",
#      ylab = "Pearson Residuals",
#      main = "Residuals vs Fitted Values")
# abline(h = 0, col = "red")
# 
# hist(residuals(glm1, type = "pearson"),
#      main = "Histogram of Pearson Residuals",
#      xlab = "Residuals",
#      breaks = 20)
# 

####
# GAM1
####

# Q-Q plot for residuals to check normality
qqnorm(resid(gam1));
qqline(resid(gam1), col = "red") 
#independencia de residuos
acf(resid(gam1), main = "ACF of Residuals")
# Compare deviance with null model
null_model <- gam(yield_kgDM_ha.1 ~ 1, family = gaussian, data = data[,-1])
anova(null_model, gam1, test = "Chisq") #< 2.2e-16 ***
# importancia relativa
varImp(gam1)
anova(gam1)
data_gam1 <- data %>% select(all_of(c('yield_kgDM_ha.1','RIPI','fcc','tas_min_spring','pH_CaCl','spei12','bio15_year')))
explainer_gam1 <- explain(gam1, data = data_gam1, y = data_gam1$yield_kgDM_ha.1)
importance_gam1 <- variable_importance(explainer_gam1)
print(importance_gam1) #perdida de RMSE


qq.gam(gam1, main= 'Q-Q plot')

plot.gam(gam1, pages=1)
plot(gam1, se = TRUE, rug = TRUE, shade = TRUE)

#interactio nterms
vis.gam(gam1, view = c("fcc", "pH_CaCl"), plot.type = "persp", theta = 215, phi = 30)


sm_gam1 <- smooth_estimates(gam1)
sm_gam1

#partial dependency plots
library(pdp)
plot(partial(gam1, pred.var = "tas_min_spring"))
plot(partial(gam1, pred.var = "RIPI"))
plot(partial(gam1, pred.var = "spei12"))
plot(partial(gam1, pred.var = "bio15_year"))
plot(partial(gam1, pred.var = "pH_CaCl"))
plot(partial(gam1, pred.var = "fcc"))

######################################################
#                                                    #
#  11.GRAFICAS Y FIGURAS                      #
#                                                    #
######################################################

mgcv::plot.gam(gam1)

# datos con prediccion
data_predict <- data 
data_predict <- data_predict %>% 
  mutate(Prediction_gam1 = predict(gam1,data_predict)) %>%
  mutate(Year = substr(Farm_code,nchar(Farm_code)-3,nchar(Farm_code))) %>%
  mutate(Plot = substr(Farm_code,1,nchar(Farm_code)-5)) %>%
  select(all_of(c('Farm_code','Plot','Year','yield_kgDM_ha.1','prediction_gam1')))

data_predict$prediction_gam1
data_predict$yield_kgDM_ha.1

# PLOT 1 : VALORES EN CADA A?O Y PARCELA

data_predict_long <- data_predict %>%
  pivot_longer(cols = c(yield_kgDM_ha.1, prediction_gam1), 
               names_to = "Type", 
               values_to = "Value") %>%
  mutate(Type = recode_factor(Type, 
                       "yield_kgDM_ha.1" = "Observados", 
                       "prediction_gam1" = "Predicci?n GAM1")) %>%
  arrange(Year, Plot)

# con facetas
ggplot(data_predict_long, aes(x = Plot, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +  # Bars side by side
  labs(x = "Parcela de estudio", y = "kg MS / ha", fill = "Valores:") +
  facet_grid(. ~ Year, scales = "free_x", space = "free_x") +  # Separate panels by Year
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
        panel.spacing = unit(0.7, "lines"),  # Increase space between facets
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), # Add border around facets
        legend.position = "bottom",  # Position legend at the bottom
        strip.text = element_text(size = 9),  # Style facet labels
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))  +  # Center title
   ggtitle("Valores de biomasa de pasto observada vs estimada en cada a?o y parcela")  # Main title

# en horizontal
# Plot with two rows for facets
# Calculate the maximum value for the x-axis limit
max_value <- max(data_predict_long$Value, na.rm = TRUE)


# PLOT 2 : VALORES RESUMEN POR PARCELA

data_predict_summary <- data_predict %>%
  pivot_longer(cols = c(yield_kgDM_ha.1, prediction_gam1), 
               names_to = "Type", 
               values_to = "Value") %>%
  mutate(Type = recode_factor(Type, 
                              "yield_kgDM_ha.1" = "Observados", 
                              "prediction_gam1" = "Predicci?n GAM1")) %>%
  group_by(Plot, Type) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    se_value = sd(Value, na.rm = TRUE) / sqrt(n())  # Standard error
  )

# Create the bar plot with error bars
ggplot(data_predict_summary, aes(x = Plot, y = mean_value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +  # Bar plot
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), 
                position = position_dodge(0.7), width = 0.2) +  # Error bars
  labs(x = "Parcela de estudio", y = "Promedio kg MS / ha ", fill = "Valores:") +
  theme_minimal()
