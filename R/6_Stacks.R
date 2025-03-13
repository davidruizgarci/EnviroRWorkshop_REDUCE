#-------------------------------------------------------------------------------
#
# Title: Stack preparation
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Author: David Ruiz-García
# Email: david.ruiz-garcia@uv.es
# Last revision: 2025/03/13
#
#-------------------------------------------------------------------------------

# A stack is a unique file that contains a collection of multiple netCDFs that share the same spatial resolution and coverage (lon and lat), .
# but they may differ in any or all of the other dimensions:
# - time
# - variable
# - depth

# In this case our stack will have a the following shared dimensions: 
# (1) lon
# (2) lat
# (3) time
# (4) depth
# but different variables (SST and CHL)


# 1. Prepare netCDFs------------------------------------------------------------
# Open them:
library(ncdf4)
library(raster)
SST_2D <- raster("input/cmems/2024/12/25/SST_Analysis_2D_2024-12-25.nc")
print(SST_2D)
plot(SST_2D)

CHL_2D <- raster("input/cmems/2024/12/25/CHL_Analysis_2D_2024-12-25.nc")
print(CHL_2D)
plot(CHL_2D)

# Make sure they are in the same spatial resolution and extent
# Spatial coverage has to be the smallest: they are equal and it is not necessary
# CHL_2D <- raster::crop(SST_2D, CHL_2D)

# Resolution has to be the broadest: we have SST with 0.083 and CHL with 0.25
# there are different resampling methods:
# 1. method = "bilinear" (Bilinear Interpolation)
# - Uses the weighted average of the four nearest neighboring cells.
# - Produces smooth results, reducing abrupt changes.
# - Best for continuous data (e.g., temperature, elevation, precipitation).
# 2. method = "ngb" (Nearest Neighbor)
# - Assigns the value of the nearest original pixel.
# - Keeps discrete values unchanged (e.g., land cover classes, categorical data).
# - Faster than bilinear interpolation but can introduce blocky artifacts.

SST_2D_resampled <- raster::resample(SST_2D, CHL_2D, method="bilinear")
print(SST_2D_resampled)
plot(SST_2D_resampled)

# Let's make sure they have the same extent after the resampling:
CHL_2D_masked <- raster::mask(CHL_2D, SST_2D_resampled)  
print(CHL_2D_masked)
print(SST_2D_resampled)

# 2. Make the stack-------------------------------------------------------------
stack <- stack(CHL_2D_masked, SST_2D_resampled) 
stack

# Save it:
# Create directory:
output_directory <- paste0("input/cmems/2024/12/25/stack")
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
# Name file:
output_file <- paste0(output_directory, "/stack_20241225.grd")
writeRaster(stack, output_file, format = "raster", overwrite = TRUE)

# Load it and check it out:
stack <- brick(output_file)
print(stack)
plot(stack$Total.Chlorophyll)
plot(stack$Temperature)


# 3. Optional: Automatise the process for large amount of files-----------------
# 3.1. Open catalog and select the dynamic variables you want to stack:
catalog <- read.csv2("input/catalog_CMEMS.csv", sep=";")
catalog$variable
variables <- c("SST", "CHL")


# 3.2. List netCDF files
# Filter only 2D ones:
day_folder <- paste0("input/cmems/2024/12/25")
nc_files <- list.files(day_folder, pattern = "_2D_.*\\.nc$", full.names = TRUE)
head(nc_files)


# 3.3. Define common resolution and extent:
# Set raster resolution and extent
# Resolution has to be the broadest: we have SST with 0.083 and CHL with 0.25
library(raster)
res <- 0.25
e <- extent(-27, -20, 14, 18) 


# 3.4. Load custom function to prepare and stack raster files for each day
source("0_customfunctions.R")

# 3.5. Prepare and save the stack:
# output path to for stack:
output_folder <- paste0("input/cmems/2024/12/25/stack")

prepareStackForDay(day_folder, variables, res, e, output_folder)


# Load it and check it out:
stack <- brick(output_file)
print(stack)
plot(stack$Total.Chlorophyll)
plot(stack$Temperature)
