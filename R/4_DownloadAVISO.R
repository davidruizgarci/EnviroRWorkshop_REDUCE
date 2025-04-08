#-------------------------------------------------------------------------------
#
# Title: Download data from AVISO
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Author: David March
# Email: david.march@uv.es
# Last revision: 2025-04-07
#
#-------------------------------------------------------------------------------



# Load libraries
library(raster)
library(ncdf4)


#---------------------------------------
# 1. Handle FSLE files from AVISO+
#---------------------------------------

# inspect nc file
ncfile <- "input/dataset-duacs-nrt-global-allsat-madt-fsle.nc4"
nc <- nc_open(ncfile)
print(nc)

# import FSLE file extracted from AVISO+ using TDS service
fsle <- raster(ncfile)
plot(fsle)

# Note range of longitude values
# Change from 0-360 to -180 - 180 degrees
fsle <- rotate(fsle)  
plot(fsle)


