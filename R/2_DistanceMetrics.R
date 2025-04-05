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

#read in dataset
dataset<-read.csv2("input/dataset.csv")

#read in bathymetry
bathy<-readRDS("output/bathy.RDS")

###distance to an isobath (in this case, to the coast) within the study area
##here start.lon and sart.lat are from your dataset, end.lon and end.lat are the closest point on the isobath

d <- dist2isobath(bathy, dataset$lon, dataset$lat, isobath = 0)

###we can add the distance to our dataset 

dataset$distance_coast<-dist2isobath(bathy, dataset$lon, dataset$lat, isobath = 0)$distance$

# Add the datapoints to base R plot
plot(bathy, image = TRUE, land = TRUE,  bpal = list(c(0, max(bathy), "#CC9C77"), c(min(bathy),0,blues)))  
points(dataset$lon, dataset$lat, pch = 21, bg = "yellow", cex = 0.8)

# Add great circle lines showing where is the closest position on the coast
linesGC(d[, 2:3], d[, 4:5])

###distance to seamounts
#for the distance to seamounts, we will be using the Yesson seamount dataset

#here is some reference materials: 
#https://www.sciencedirect.com/science/article/abs/pii/S0967063711000392

##bring in seamount data from Yesson et al. 2011
seamounts <- read.csv2("input/Seamounts_Yesson.csv")
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
