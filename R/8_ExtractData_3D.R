#-------------------------------------------------------------------------------
#
# Title: Extraction of Environmental data from 3D rasters
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Author: David Ruiz-García
# Email: david.ruiz-garcia@uv.es
# Last revision: 2025/03/13
#
#-------------------------------------------------------------------------------

# 1. Load data------------------------------------------------------------------
data <- read.csv("input/dataset.csv", sep = ";") #remember having date format in your .csv
head(data)


# 2. Explore temporal and spatial range-----------------------------------------
# Make sure of formates for dates and numerical for lon and lat
library(dplyr)
data <- data %>%
  mutate(
    date = as.Date(date),  # Convert to Date format
    lon = as.numeric(gsub(",", ".", lon)),  # Replace commas with dots and convert to numeric
    lat = as.numeric(gsub(",", ".", lat)),  
    depth = as.numeric(gsub(",", ".", depth)))
str(data)

range(data$date)
range(data$lon)
range(data$lat)
range(data$depth)

# 3. Source 3D variables in catalog---------------------------------------------
#open catalog
catalog <- read.csv("input/Catalog_CMEMS.csv", sep=";")

cat <- catalog %>%
  filter(dimensions %in% c("3D")) #, variable %in% c("o2")
head(cat)

# 2. Extract your own data------------------------------------------------------
source("0_customfunctions.R")

# Repository to folder where netCDFs are:
repo <- paste0("input/cmems") 
# Get the ID(s) of the product(s) you are going to extract data from:
productid <- cat$id_product[1]

# The custom function (cmems3d_all) uses the following parameters that you need to provide:
# data        Your dataset name
# lon         longitude column in your dataset
# lat         latitude column in your dataset
# date        POSIXct date time or Date column in your dataset
# id          identification column for each observation/position/fishing operation in your dataset
# depth       depth value, in meters (positive) column in your dataset
# productid   id of the product from catalog table. This is used to find the netcdf file from the repository (repo)
# repo        repository path with netcdf files. 

# Use function for extraction
data <- cmems3d_all(lon=data$lon, lat=data$lat, date=data$date, productid=productid, repo=repo, id=data$observation, data=data)
head(data)

#You will obtain three different columns:
#seasurface: value of the variable at surface
#seabottom: value of the variable at seabottom
#nearest: value of the variable at the depth of the netCDF closest to the depth of your observation



# Save dataframe
write.csv(data, "output/dataset_3D.csv", row.names = FALSE)



