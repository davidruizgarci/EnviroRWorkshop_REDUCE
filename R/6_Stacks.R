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
output_directory <- paste0(input_directory, "/stack")
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


# 3.2. List natCDF files
# Filter only 2D ones:
nc_files <- list.files(day_folder, pattern = "_2D_.*\\.nc$", full.names = TRUE)
head(nc_files)


# 3.3. Define common resolution and extent:
# Set raster resolution and extent
# Resolution has to be the broadest: we have SST with 0.083 and CHL with 0.25
library(raster)
res <- 0.25
e <- extent(-27, -20, 14, 18) 


# 3.4. Load custom function to prepare and stack raster files for each day
prepareStackForDay <- function(day_folder, variables, res, e, output_folder) {
  # Define extent and resolution
  e <- extent(e)
  
  # Create an empty stack
  stack_dynamic <- stack()
  
  # Mapping of original variable names to new names
  variable_names_map <- list(
    CHL = "Total.Chlorophyll",
    SST  = "Temperature")
  
  for (variable in variables) {
    # example to test code: variable <- variables[2]
    
    # Construct the file pattern for the variable
    file_pattern <- paste0("^", variable, "_Analysis_2D_.*\\.nc$")
    
    # List netCDF files for the given variable
    nc_files <- list.files(path = input_directory, pattern = file_pattern, full.names = TRUE)
    
    if (length(nc_files) == 0) {
      next
    }
    
    # Read each netCDF file and prepare the raster
    for (nc_file in nc_files) {
      # example for testing code: nc_file <- nc_files[1]
      
      # Open the netCDF file
      r <- raster(nc_file)
      
      # Calculate the number of columns and rows
      ncol <- round((e@xmax - e@xmin) / res)
      nrow <- round((e@ymax - e@ymin) / res)
      
      # Create an empty raster with the specified extent and resolution
      target_raster <- raster(ncol = ncol, nrow = nrow, 
                              xmn = e@xmin, xmx = e@xmax, 
                              ymn = e@ymin, ymx = e@ymax)
      
      r_resampled <- resample(r, target_raster, method = "bilinear")

      # Stack the raster
      stack_dynamic <- stack(stack_dynamic, r_resampled)
      
      # Rename the latest raster layer in the stack with the desired name
      layer_name <- variable_names_map[[variable]]
      names(stack_dynamic)[nlayers(stack_dynamic)] <- layer_name
      
      
      # Close the netCDF file
      rm(r)
    }
  }
  # Save the final stack to file
  if (nlayers(stack_dynamic) > 0) {
    # Extract the base directory and split by '/'
    components <- unlist(strsplit(day_folder, "/"))
    
    # Assumes that folder structure includes year, month, day in the specified positions
    year <- components[3]
    month <- components[4]
    day <- components[5]
    
    # Create a date string for the file name
    date_string <- paste0(year, month, day)
    
    # Define the output file path
    output_file <- file.path(output_folder, paste0("stack_", date_string, ".grd"))
    
    # Save the final stack
    writeRaster(stack_dynamic, output_file, format = "raster", overwrite = TRUE)
    
    cat("Stack saved to", output_file, "\n")
  }
  
  # Return the final stacked raster
  return(stack_dynamic)
}


# 3.5. Prepare stack:
# path to environmental data:
day_folder <- paste0("input/cmems/2024/12/25")
output_folder <- paste0("input/cmems/2024/12/25/stack")

prepareStackForDay(day_folder, variables, res, e, output_folder)
stack_dynamic

# Load it and check it out:
stack <- brick(output_file)
print(stack)
plot(stack$Total.Chlorophyll)
plot(stack$Temperature)
