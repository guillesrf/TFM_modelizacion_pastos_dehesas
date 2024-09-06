Author: Guillermo Ramírez Ferrer
This repository contains the scripts for reproducign the methodology of my Master's thesis, "Modelización de la calidad de pastos en dehesas usando herramientas de teledetección y aplicación en escenarios de cambio climático".
The scripts allow to obtain a predictive model of spring herbaceous pasture dry matter yield in the Iberian dehesas, from the ground truth biomass data sampled at the sites described in Pulina et al., 2018, Carrasco et al. 2015; López-Sánchez et al. 2016 and 

To reproduce this workflow reproducible; it is necessary to download additional data not present in this repository:
- DEM and DVM of the study area from PNOA from https://centrodedescargas.cnig.es/CentroDescargas/index.jsp 
- Chemical and physical soil properties maps from the ESDAC collection:
https://esdac.jrc.ec.europa.eu/content/topsoil-physical-properties-europe-based-lucas-topsoil-data
https://esdac.jrc.ec.europa.eu/content/chemical-properties-european-scale-based-lucas-topsoil-data
- Climatic data for historic and future reference (ssp585, GDFL-ESM4) periods can be obtained with Windows-WGET and executing the script_SHELL in the Windows Shell console. For this example, at least 240 GB of free space are required.
- Future SEP index projections (ssp585, GDFL-ESM4) data from the Global Multiscale SPEI Dataset at https://dds.cmcc.it/#/dataset/global-spei-ensemble-simulations/future_2040_2079
  
The multispectral images and multispectral csv data in this example refers to the study sites before mentioned. You can obtain your own images using the scripts and assets decribed in scripts_GEE using the Google Earth Engine service.

Once these input data files are prepared, use the R scripts 1-5 can be executed:
1_time_series: Execute timeSeries.R R to obtain the spring trend of NDVI index to select the threshold for image filtering in the dehesas_imagenes_batch GEE script
2_mdv_mask: Execute mdv_mask.R to process the DVM files into a binary tree mask at 2,5 m resolution, used for later scripts and the dehesa_indexes GEE script.
3_varExp_otras: Process the climatic (clim_raster_toDF.R) ,SPEI (climDF_to_SPEI.R),soil (soil_raster_toDF.R) and topographic (terrain_raster_toDF.R) variables from raw raster data.
4_dataframetomodel: stack each explanatory variable, obtain, validate and plot several models with dataframetomodel_batch.R. For more efficient stepwise selection using ols_step_best_subset (olsrr package), run the paralel_subset_model.R script
5_predictions: prediction_atoquedo.R creates a prediction based on the average values in a whole study plot, and also produces pixel-level yield maps. prediction_ssp585 generates a prediction based on future climatic projections based on ssp585 with the GDFL-ESM4 model.

Contact:guillesrf@gmail.com
