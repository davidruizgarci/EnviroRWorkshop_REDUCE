#-------------------------------------------------------------------------------
#
# Title: Extraction of Environmental data from 2D rasters
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Author: Sarah Saldanha
# Email: sarahsaldanha9@gmail.com
# Last revision: 2025-04-05
#
#-------------------------------------------------------------------------------

library(raster)
library(dplyr)
library(exactextractr)
library(sf)

##first bring in the point data
dataset<-read.csv2("input/dataset.csv")

#turn dataset into a simple feature

###duplicate the lat lon variables so that we keep them in the dataset 
dataset$Longitude<-dataset$lon
dataset$Latitude<-dataset$lat

dataset_sf <- st_as_sf(dataset, coords = c("Longitude", "Latitude"), 
                   crs = 4326, agr = "constant")

##bring in the environmental data brick that we created previously
stack <- brick(output_file)
  
### extract values for the exact points
dataset_sf$Total.Chlorophyll<- extract(stack$Total.Chlorophyll, dataset_sf)
dataset_sf$Temperature<- extract(stack$Temperature, dataset_sf)
plot(stack$Total.Chlorophyll)
  
### extract means, sd, min, max of values per buffer around points. 
    
###distance here is in degrees since our data is in lat long 
### since the resolution is 0.25 (approx 27km), we will need to do a buffer larger than this to get the metrics. 
#Here we will create a buffer of 100 km (100 000m)
    
dataset_sf$Total.Chlorophyll_buffer_mean<-extract(stack$Total.Chlorophyll,dataset_sf, buffer=100000, fun=mean)
dataset_sf$Total.Chlorophyll_buffer_sd<-extract(stack$Total.Chlorophyll,dataset_sf, buffer=100000, fun=sd) ### 
dataset_sf$Total.Chlorophyll_buffer_min<-extract(stack$Total.Chlorophyll,dataset_sf, buffer=100000, fun=min)
dataset_sf$Total.Chlorophyll_buffer_max<-extract(stack$Total.Chlorophyll,dataset_sf, buffer=100000, fun=max)
        
###do the same with exactextract, which handles grid cells that are partially covered by a polygon
###create buffer first
sp_buffer <- st_buffer(dataset_sf, 100000) 
    
###and then extract
dataset_sf$Total.Chlorophyll_buffer_raster_extract_mean<-exactextractr::exact_extract(stack$Total.Chlorophyll,sp_buffer,'mean')
dataset_sf$Total.Chlorophyll_buffer_raster_extract_sd<-exactextractr::exact_extract(stack$Total.Chlorophyll,sp_buffer,'sd')
dataset_sf$Total.Chlorophyll_buffer_raster_extract_min<-exactextractr::exact_extract(stack$Total.Chlorophyll,sp_buffer,'min')
dataset_sf$Total.Chlorophyll_buffer_raster_extract_max<-exactextractr::exact_extract(stack$Total.Chlorophyll,sp_buffer,'max')

plot(dataset_sf$Total.Chlorophyll_buffer_raster_extract_mean, dataset_sf$Total.Chlorophyll_buffer_mean) ## simialr but a bit different
