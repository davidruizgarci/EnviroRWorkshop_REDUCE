#-------------------------------------------------------------------------------
#
# Title: Derivation of products
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Author: David March
# Email: david.march@uv.es
# Last revision: 2025-04-07 
#
#-------------------------------------------------------------------------------


# Load libraries
library(raster)


#-------------------------------------------
# 1. Log-transform data
#-------------------------------------------

# import CHL
chl <- raster("input/CHL_Analysis_2D_2024-12-25.nc")
plot(chl)
hist(chl)

# log-trandform data
chl_log <- log1p(chl)
plot(chl_log)
hist(chl_log)


#-------------------------------------------
# 2. Derive gradients
#-------------------------------------------

# import temperature map
r <- raster("input/SST_Analysis_2D_2024-12-25.nc")
plot(r)

# where do you think you will get a stronger gradient?


# derive gradient
# use the slope function to calculate the slope of the raster
r_grad <- terrain(r, opt = "slope", unit = "degrees", neighbors = 4)
plot(r_grad)



#-------------------------------------------
# 3. Eddy kinetic energy
#-------------------------------------------

# import u component
# horizontal flow speed of water in an east-west direction
u <- raster("input/uo_Analysis_2024-12-25.nc") 
plot(u)

# import v component
# vertical flow speed of water in a north-south direction
v <- raster("input/vo_Analysis_2024-12-25.nc")
plot(v)

# calculate the eddy kinetic energy (EKE)
eke <- (u^2 + v^2)/2
plot(eke, main = "Eddy Kinetic Energy")

# what are the units of EKE?
