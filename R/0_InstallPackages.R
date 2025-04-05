#-------------------------------------------------------------------------------
#
# Title: Requiered packages
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Authors: Sarah Saldanha, David March, David Ruiz-García
# Last revision: 2025/03/13
#
#-------------------------------------------------------------------------------

# List of packages that need to be installed before the course:
install.packages(c("dplyr", "reticulate", "ncdf4", "raster", "marmap", "ggplot2", "sf", "exactextractr", "fossil"))


#' For accessing Copernicus Marine Science (CMEMS) we need to access through python:
#' Even if the copernicusmarine package was written in Python, 
#' it doesn't mean you necessarily have to use the Copernicus Marine Toolbox in Python. 
#' You can call all functions in a Command Line Interface (CLI) 
#' and then use your favorite language for post-processing the data. 
#' For accessing we need to run the following code:

# Step 1: Install python:
library(reticulate)
install_python() 

# Step 2: Install the Copernicus Marine Toolbox in a virtual environment
virtualenv_create(envname = "cmems")
virtualenv_install("cmems", packages = c("copernicusmarine"))
reticulate::use_virtualenv("cmems", required = TRUE)
# load package / import library (py)
cm <- import("copernicusmarine")
# log in in your CMEMS user (you should have another script with this info)
cm$login(username, password)
# for session yes == y
y

