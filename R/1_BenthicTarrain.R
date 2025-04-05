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
#bring in libraries 
library(marmap)
library(raster)
library(dplyr)
library(ggplot2) 
library(sf)


#Open your dataset
dataset<-read.csv2("input/dataset.csv")
view(dataset)

#download the bathymetry for the area of your study using marmap


#This will take a while if the area is quite large and requires internet connection.
#Here we are downloading an area 0.5 degrees larger than the extent of the data, I recommend this
#especially if you are planning to extract the bathymetry within a kernel or buffer of your points
bathy <- getNOAA.bathy(lon1 = min(dataset$lon)-0.5, lon2 = max(dataset$lon)+0.5,
                       lat1 = min(dataset$lat)-0.5, lat2 =  max(dataset$lat)+0.5, resolution = 1)

##have a look at the bathymetry lines
plot(bathy)

#bathymetry features from marmap can be plotted in both base R and ggplot2. Here are some examples on how you
#you can play

###you can also play with the colors of the map here 
blues <- colorRampPalette(c("red","purple","blue", "cadetblue1","white"))
plot(bathy, image = TRUE, bpal = blues(100))

e bpal argument of plot.bathy() also accepts a list of depth/altitude slices associated with a set of colors for each slice. This method makes it possible to easily produce publication-quality maps. For instance, using the bathy dataset downloaded at full resolution (i.e. with the resolution argument of the getNOAA.bathy() function set to 1) we can easily produce a high-resolution map: 

# Creating a custom palette of blues 
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1") 

#Plotting the bathymetry with different colors for land and sea 
plot(bathy, image = TRUE, land = TRUE, lwd = 0.1, bpal = list(c(0, max(bathy), "#CC9C77"), c(min(bathy),0,blues)))

# Making the coastline more visible plot
plot(bathy, deep = 0, shallow = 0, step = 0, lwd = 0.4, add = TRUE)


###you can also transform the bathymetric data into a raster

bathy_ras<-as.raster(bathy)

plot(bathy_ras)

###or as a spatial grid dataframe

bathy_sp<-as.SpatialGridDataFrame(bathy)

plot(bathy_sp)



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
  

##which positions are above seamounts? Let's say we consider points less than 60km from the peak of seamounts as on seamounts
  
  n_pos_60km <-filter(dataset_rep, dataset_rep$dist_seamount < 60.001) %>% group_by(observation) %>% summarize(n_seamount_60_1500 = n())
  
  dataset_seamount <- left_join(dataset, n_pos_60km, by = c("observation"))
  
  dataset_seamount$seamount_60_1500 <- case_when(is.na(dataset_seamount$n_seamount_60_1500) == FALSE  ~ "Yes",
                                                   is.na(dataset_seamount$n_seamount_60_1500) == TRUE  ~ "No",)
  
  ##clean up working directory. 
  rm(seamount_rep, dataset_rep, n_pos_60km,sm_shallow_1500)
