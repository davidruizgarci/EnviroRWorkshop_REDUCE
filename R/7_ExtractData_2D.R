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
library(ncdf4)
library(lubridate)

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



#Now lets extract from a dynamic raster


##

ncfile <- "input/cmems_mod_glo_phy_my_0.083deg_P1D-m_1744046380344.nc"

nc <- nc_open(ncfile)

nc ###what variables and dimensions are included here? 


###select one variable to create a raster brick

names(nc[['var']])

rast<- raster::brick(ncfile, varname = "so") #which one did I select here? 

plot(rast[[1]])
plot(rast[[10]])

###now extract by date, lat and lon for my points

dataset2<-read.csv("input/dataset2.csv")
dataset2
str(dataset2)

library(lubridate)
dataset2$date<-ymd(dataset2$date)

table(dataset2$date) ##two dates in this dataset that are repeated

##duplicate lat and lon
dataset2$Longitude<-dataset2$lon
dataset2$Latitude<-dataset2$lat


dataset2_sf <- st_as_sf(dataset2, coords = c("Longitude", "Latitude"), 
                      crs = 4326, agr = "constant")





##plot positions on this 
plot(rast[[1]])
points(dataset2_sf$lon, dataset2_sf$lat, pch = 21, bg = "red", cex = 0.8)


###there is a time dimension in this raster
getZ(rast) 

##subset to the dates that we need? 
rast_sub<-subset(rast, which(getZ(rast) %in% unique(dataset2$date)))

getZ(rast_sub) #time dimension is gone! ###


###split dataset by date
selection_list <- split( dataset2_sf , f = dataset2_sf$date)

###Lets try to extract the values for one of the days

  uni_dates<-unique(dataset2$date)[2]
  r<-subset(rast, which(getZ(rast) == uni_dates))
  points<-selection_list[names(selection_list) == as.character(uni_dates)][[1]]
  points$Salinity <- extract(r, points)

  
###this is a strange error since usually raster::extract can deal with NA's... but since I was not able to figure out why,
##in this particular case, we are using a ChatGPT suggested workaround...setting NA'S as -9999


# Check the current nodata value
NAvalue(rast)

# Set a nodata value if it is missing or incorrect (e.g., -9999)
NAvalue(rast) <- -9999


uni_dates<-unique(dataset2$date)[2]
r<-subset(rast, which(getZ(rast) == uni_dates))
points<-selection_list[names(selection_list) == as.character(uni_dates)][[1]]
points$Salinity <- extract(r, points)
  

