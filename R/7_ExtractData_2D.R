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
dataset_sf$Total.Chlorophyll_buffer_raster_extract_sd<-exactextractr::exact_extract(stack$Total.Chlorophyll,sp_buffer, 'stdev')
dataset_sf$Total.Chlorophyll_buffer_raster_extract_min<-exactextractr::exact_extract(stack$Total.Chlorophyll,sp_buffer,'min')
dataset_sf$Total.Chlorophyll_buffer_raster_extract_max<-exactextractr::exact_extract(stack$Total.Chlorophyll,sp_buffer,'max')

plot(dataset_sf$Total.Chlorophyll_buffer_raster_extract_mean, dataset_sf$Total.Chlorophyll_buffer_mean) ## simialr but a bit different

##there are many other metrics that can be extracted using exactextract: 

# count:	Sum of all cell coverage fractions.
# majority (or mode):	The raster value with the largest sum of coverage fractions.
# max:	Maximum value of cells that intersect the polygon, ignoring coverage fractions.
# mean:	Mean value of cells that intersect the polygon, weighted by the fraction of the cell that is covered
# median:	Median value of cells that intersect the polygon, weighted by the fraction of the cell that is covered.
# quantile:	Arbitrary quantile value of cells that intersect the polygon, weighted by the fraction of the cell that is covered.
# min:	Minimum value of cells that intersect the polygon, ignoring coverage fractions.
# minority:	The raster value with the smallest sum of coverage fractions.
# sum:	Sum of values of raster cells that intersect the polygon, with each raster value weighted by its coverage fraction.
# variety:	The number of distinct raster values in cells wholly or partially covered by the polygon.
# variance:	The population variance of cell values, weighted by the fraction of each cell that is covered by the polygon.
# stdev:	The population standard deviation of cell values, weighted by the fraction of each cell that is covered by the polygon.
# coefficient_of_variation:	The population coefficient of variation of cell values, weighted by the fraction of each cell that is covered by the polygon.
# frac:	Fraction of covered cells that are occupied by each distinct raster value.
# 
# Three additional summary operations require the use of a second weighting raster, provided in the weights argument to exact_extract:
#   
# weighted_mean: Mean value of defined (non-NA) cells that intersect the polygon, weighted by the product of the coverage fraction and the value of a second weighting raster.
# weighted_sum: Sum of defined (non-NA) values of raster cells that intersect the polygon, multiplied by the coverage fraction and the value of a second weighting raster.
# weighted_variance:	Population variance of defined (non-NA) values of cells that intersect the polygon, weighted by the product of the coverage fraction and the value of a second weighting raster.
# weighted_stdev:	Population standard deviation of defined (non-NA) values of raster cells that intersect the polygon, multiplied by the coverage fraction and the value of a second weighting raster.
# weighted_frac:	Fraction of covered cells that are occupied by each distinct raster value, with coverage fractions multiplied by the value of a second weighting raster.
