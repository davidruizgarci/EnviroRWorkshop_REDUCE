#-------------------------------------------------------------------------------
#
# Title: Distance metrics
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Author: Sarah Saldanha
# Email: sarahsaldanha9@gmail.com
# Last revision: 2025-04-05
#
#-------------------------------------------------------------------------------

###Rclass draft 
library(marmap)
library(raster)
library(dplyr)
library(ggplot2) 
library(exactextractr)
library(sf)


dataset<-read.csv2("input/dataset.csv")



###distance to an isobath (in this case, to the coast) within the study area
##here start.lon and sart.lat are from your dataset, end.lon and end.lat are the closest point on the isobath

d <- dist2isobath(bathy, dataset$lon, dataset$lat, isobath = 0)





plot(bathy, image = TRUE, lwd = 0.1, land = TRUE, bpal = list(c(0, max(bathy), "grey"), c(min(bathy), 0, blues)))
# Make the coastline more visible
plot(bathy, deep = 0, shallow = 0, step = 0, lwd = 0.6, add = TRUE) # Add the 5 points 
points(dataset$lon, dataset$lat, pch = 21, bg = "orange2", cex = 0.8)
# Add great circle lines
linesGC(d[, 2:3], d[, 4:5])

####plotting bathymetry in ggplot with point from dataset

###set point as Simple feature

###duplicate the lat lon variables so that we keep them in the dataset 
dataset$Longitude<-dataset$lon
dataset$Latitude<-dataset$lat

dataset_sf <- st_as_sf(dataset, coords = c("Longitude", "Latitude"), 
                   crs = 4326, agr = "constant")


ggplot(bathy, aes(x=x, y=y)) + coord_quickmap() +
  # background
  geom_raster(aes(fill=z)) +
  scale_fill_etopo() + # countours 
  geom_contour(aes(z=z), breaks=c(0,-1500,-3000,-4500,-6000,-7500,-9000), colour="black", size=0.2 ) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0))+
  geom_sf(data = dataset_sf, aes(x = lon, y = lat), color = "red", size = 2)

  
###Extract depth for your points

get.depth(bathy, x=dataset$lon, y=dataset$lat, locator=FALSE)

### add this to your dataset

dataset$depth<-get.depth(bathy, x=dataset$lon, y=dataset$lat, locator=FALSE)$depth


###extract characteristics of the terrain based on 8 surrounding cells

#slope: Angle of steepness of bathymetry
#aspect: the compass direction that a slope faces,  measured in degrees from north
#roughness: Roughness is the difference between the maximum and the minimum value of a cell and its 8 surrounding cells.
#TRI (Terrain Ruggedness Index) is the mean of the absolute differences between the value of a cell and the value of its 8 surrounding cells.
#TPI (Topographic Position Index) is the difference between the value of a cell and the mean value of its 8 surrounding cells. 

terrain_data <- terrain(bathy_ras, opt = c("slope", "aspect", "roughness", "TRI", "TPI"), unit = "degrees")

plot(terrain_data$slope, main = "Slope")
plot(terrain_data$aspect, main = "Aspect")
plot(terrain_data$roughness, main = "Roughness")
plot(terrain_data$tri, main = "TRI")
plot(terrain_data$tpi, main = "TPI")


###extract these values for our points 

dataset$slope <- extract(terrain_data$slope, dataset_sf)
dataset$aspect <- extract(terrain_data$aspect, dataset_sf)
dataset$roughness <- extract(terrain_data$roughness, dataset_sf)
dataset$TRI <- extract(terrain_data$tri, dataset_sf)
dataset$sTPI <- extract(terrain_data$tpi, dataset_sf)


###distance to seamounts

##bring in seamount data from Yesson et al. 

seamounts <- read.csv2("Seamounts_Yesson.csv")
seamounts$lon <- as.numeric(as.character(seamounts$X))
seamounts$lat <- as.numeric(as.character(seamounts$Y))
seamounts$X <- seamounts$Y <- NULL


###first select seamount that are within the study area 

  seamounts_cut<-subset(seamounts, lon > min(dataset$lon)-1 & lon <max(dataset$lon) +1 & lat< max(dataset$lat)+1 & lat > min (dataset$lat)-1)
  
###different definitions on what a seamount is. Here we are going with seamounts that are less than 1500m deep
  
  sm_shallow_1500 <- subset(seamounts_cut, seamounts_cut$Depth > -1501)

  
  rm(seamounts_cut)
  
# replicte the rows of the dataset based on the number of seamounts at 1500 and 500m 
  dataset_rep <- do.call(rbind, replicate(nrow(sm_shallow_1500), dataset, simplify=FALSE)) #seamounts
  seamount_rep <- do.call(rbind, replicate(nrow(dataset), sm_shallow_1500, simplify=FALSE))#nrows
  
  seamount_rep <- dplyr::arrange(seamount_rep, seamount_rep$PeakID)
  
  
  dataset_rep$seamountID <- seamount_rep$PeakID
  dataset_rep$seamount_lat <- seamount_rep$lat
  dataset_rep$seamount_lon <- seamount_rep$lon
  
  #####calculate the distance of all seamounts
  dataset_rep$dist_seamount <- fossil::deg.dist(dataset_rep$lon, dataset_rep$lat, dataset_rep$seamount_lon, dataset_rep$seamount_lat)
  

##which positions are above seamounts? Lets say we consider point less than 60km from seamounts as on seamounts
  
  n_pos_60km <-filter(dataset_rep, dataset_rep$dist_seamount < 60.001) %>% group_by(observation) %>% summarize(n_seamount_60_1500 = n())

  
  
  dataset_seamount <- left_join(dataset, n_pos_60km, by = c("observation"))
  
  dataset_seamount$seamount_60_1500 <- case_when(is.na(dataset_seamount$n_seamount_60_1500) == FALSE  ~ "Yes",
                                                   is.na(dataset_seamount$n_seamount_60_1500) == TRUE  ~ "No",)
  

  ##clean up working directory. 
  
  rm(seamount_rep, dataset_rep, n_pos_60km,sm_shallow_1500)

  
  
