#-------------------------------------------------------------------------------
#
# Title: Benthic Terrain Modelling
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Author: Sarah Saldanha
# Email: sarahsaldanha9@gmail.com
# Last revision: 2025-04-05
#
#-------------------------------------------------------------------------------

#Open your dataset
dataset<-read.csv2("dataset.csv")
view(dataset)

#download the bathymetry for the area of your study. 
#This will take a while if the area is quite large and requires internet connection.
#Here we are downloading an area 0.5 degrees larger than the extent of the data, I recommend to do this 
bathy <- getNOAA.bathy(lon1 = min(dataset$lon)-0.5, lon2 = max(dataset$lon)+0.5,
                       lat1 = min(dataset$lat)-0.5, lat2 =  max(dataset$lat)+0.5, resolution = 1)

##have a look at the bathymetry lines
plot(bathy)

