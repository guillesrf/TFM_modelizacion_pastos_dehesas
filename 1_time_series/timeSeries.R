library(dyplr)
library(tidyverse)
library(lubridate)
library(ggplot2)

dir <- 'C:/TFM/ScriptsR/1_time_series'
setwd(dir)

# poner nombre de meses en castellano
Sys.setlocale("LC_ALL", "Spanish") 

#
df <- read.csv('./1_DATA/time_series_NDVI_buffer1000.csv',header=TRUE,sep=',')
df <- df %>% drop_na()
df$aoi <- substr(df$aoi, 1, nchar(df$aoi) - 6)
df$aoi <- as.factor(df$aoi)
df$month <- as.factor(month(df$date))
df$month_year <- as.factor(paste0(month(df$date),'_',year(df$date)))
df$date <- as.Date(df$date)
str(df)

##### PLOT

#Promedio por mes de cada parcela y cada año
# df_eachPlot_eachYear <- df %>% group_by(across(c(aoi,month_year))) %>% summarise(ndvi_mean = mean(ndvi_mean),.groups = 'drop')
# 
# df_eachPlot_eachYear$month_year <- as.Date(
#   paste0("01-", gsub("_(\\d+)", "-\\1", df_eachPlot_eachYear$month_year)), 
#   format="%d-%m-%Y")
#   
# g_eP_eY <- df_eachPlot_eachYear %>%
#     ggplot(aes(x=month_year, y=ndvi_mean, colour=aoi))+
#     geom_point() +
#     geom_line()+
#     scale_x_date(date_labels="%b %d", date_breaks='3 month')+
#     labs(
#       x = "Mes",
#       y = "NDVI promedio",
#       title = "Evolución NDVI") +
#     theme_minimal()+
#     theme(axis.text.x=element_text(angle=60, hjust=1))+
#     theme(plot.title=element_text(hjust=0.5))
# plot(g_eP_eY)


#Promedios por mes, de cada parcela, todos los años juntos
df_eachPlot_allYears <- df %>% group_by(across(c(aoi,month))) %>% summarise(ndvi_mean = mean(ndvi_mean),.groups = 'drop')

df_eachPlot_allYears$year <- 2030 # Añadimos un año cualquiera
df_eachPlot_allYears$date <- as.Date(paste(df_eachPlot_allYears$year, df_eachPlot_allYears$month, "01", sep = "-"), format = "%Y-%m-%d")


g_eP_aY <- df_eachPlot_allYears %>% 
  ggplot(aes(x=date, y=ndvi_mean, colour=aoi))+
  geom_point()+
  geom_line()+
  scale_x_date(date_labels="%B", date_breaks='1 month')+
  labs(
    x = "Mes",
    y = "NDVI promedio",
    title = "Evolución NDVI") +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(plot.title=element_text(hjust=0.5))
plot(g_eP_aY)


#Promedios por mes, todas las parcelas juntas, en cada año
# df_allPlots_eachYear <- df %>% group_by(month_year) %>% summarise(ndvi_mean = mean(ndvi_mean),.groups = 'drop')
# df_allPlots_eachYear <- df_allPlots_eachYear %>%
#   mutate(
#     # Extract month (numeric part before the underscore)
#     month = as.numeric(sub("_(\\d+)$", "", month_year)),
#     # Extract year (numeric part after the underscore)
#     year = as.numeric(sub("^(\\d+)_", "", month_year)),
#     # Create a Date column using the extracted month and year
#     date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")
#   )
# 
# 
# g_aP_eY <- df_allPlots_eachYear %>%
#   ggplot(aes(x=date, y=ndvi_mean,))+
#   geom_line()+
#   geom_point() +
#   scale_x_date(date_labels="%B", date_breaks='3 month')+
#   labs(
#     x = "Mes",
#     y = "NDVI promedio",
#     title = "Evolución NDVI") +
#   theme_minimal()+
#   theme(axis.text.x=element_text(angle=60, hjust=1))+
#   theme(plot.title=element_text(hjust=0.5))+
#   facet_grid(rows = vars(year))
# plot(g_aP_eY)

#Promedios por mes, de todas las parcelas y todos los años juntos
df_allPlots_allYears <- df %>% group_by(month) %>% summarise(ndvi_mean = mean(ndvi_mean),.groups = 'drop')

df_allPlots_allYears$year <- 2030  # Año cualquiera
df_allPlots_allYears$date <- as.Date(paste(df_allPlots_allYears$year, df_allPlots_allYears$month, "01", sep = "-"), format = "%Y-%m-%d")

# g_aP_aY <- df_allPlots_allYears %>%
#   ggplot(aes(x = date, y = ndvi_mean)) +
#   geom_line() +
#   geom_point() +
#   scale_x_date(date_labels = "%B", date_breaks = '1 month') +
#   labs(
#     x = "Mes",
#     y = "NDVI promedio",
#     title = "Evolución NDVI"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#   theme(plot.title = element_text(hjust = 0.5))
g_aP_aY <- df_allPlots_allYears %>%
  ggplot(aes(x = date, y = ndvi_mean)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k= 1), color = "blue") +
  scale_x_date(date_labels = "%B", date_breaks = '1 month') +
  labs(
    x = "Mes",
    y = "NDVI promedio",
    title = "Evolución NDVI"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

print(g_aP_aY)


#### TIME SERIES

