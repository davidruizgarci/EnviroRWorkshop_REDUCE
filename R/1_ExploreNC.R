#-------------------------------------------------------------------------------
#
# Title: Explore NetCDF
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Author: David March
# Email: david.march@uv.es
# Last revision: 2025-04-06
#
#-------------------------------------------------------------------------------


# Load libraries
library(ncdf4)
library(raster)
library(rasterVis)

#---------------------------------------
# 1. Inspect a NetCDF
#---------------------------------------

# Set the path for the NetCDF file
ncfile <- "input/global-analysis-forecast-phy-001-024-monthly_1624214790015.nc"

# Download CMEMS sample data
download.file(url = "https://github.com/dmarch/Rworkshop-MarineData4America/raw/main/data/global-analysis-forecast-phy-001-024-monthly_1624214790015.nc",
              destfile = ncfile,
              mode = "wb")

# Import NetCDF
nc <- nc_open(ncfile)

# Print information about the NetCDF file
print(nc)



#-------------------------------------------
# 2. Extract data from NetCDF
#-------------------------------------------

# Variable:
temp <- ncvar_get(nc, varid = "thetao")
class(temp)
dim(temp)
temp[1:5, 1:5, 1]

# Dimension: extract time values
time <- nc$dim$time$vals
print(time)

# Transform to time class
# - Convert time from hours (as defined in metadata) to seconds (unit used by the function)
# - Define origin date (found in metadata)
time <- as.POSIXct(time * 3600, origin = "1950-01-01", tz = "UTC") 
print(time)



#-------------------------------------------
# 3. Import a NetCDF as single band layer
#-------------------------------------------

# import NetCDF with raster
sst_single <- raster(ncfile)

# print a summary of the raster
sst_single

# plot raster dataset
plot(sst_single)


#-------------------------------------------
# 4. Import a NetCDF as multiband band layer
#-------------------------------------------

# import multi-band NetCDF file
sst_multi <- brick(ncfile)

# print a summary of the brick
sst_multi

# plot brick dataset
levelplot(sst_multi)
