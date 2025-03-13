#-------------------------------------------------------------------------------
#
# Title: Download data from CMEMS
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Author: David Ruiz-García
# Email: david.ruiz-garcia@uv.es
# Last revision: 2025/03/13
#
#-------------------------------------------------------------------------------

# 1. Set main data paths--------------------------------------------------------

# 1.1. Write the path to the folder where you have your data:
main_dir <- "C:/Users/david/OneDrive/Escritorio/EnviroRWorkshop_REDUCE"
# If directory doesn't exist yet, create it:
if (!dir.exists(main_dir)) dir.create(main_dir, recursive = TRUE)
# Set as main directory (so you don't need to write the full path anymore)
setwd(main_dir)


# 1.2. Create data paths:
# input is where you will save your data
input_data <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_data)) dir.create(input_data, recursive = TRUE)

# output is where you will save the analyses done based on your data
output_data <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# 1.3. load CEMEMS username / password
# To avoid writing your user and password here (visible to anyone with access) keep it a txt file
path <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/user.txt"
username <- paste(readLines(path, warn = FALSE), collapse = "")
path <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/psw.txt"
password <- paste(readLines(path, warn = FALSE), collapse = "")


# 2.Prepare your dataset--------------------------------------------------------
# We are going to work with a daily resolution, then we want to adjust the download
# to those days in which you have data. 











# 2. Download environmental data from CMEMS-------------------------------------

# 2.1. Import data catalog
# Remember, the catalog is where you have the required information for download
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
# Check it out and ensure numerical variables are numeric
str(catalog) 

# Convert to numerical if they aren't:
library(dplyr)
catalog <- catalog %>%
  mutate(
    xmin = as.numeric(gsub(",", ".", xmin)),
    xmax = as.numeric(gsub(",", ".", xmax)),
    ymin = as.numeric(gsub(",", ".", ymin)),
    ymax = as.numeric(gsub(",", ".", ymax)),
    depth_min = as.numeric(gsub(",", ".", depth_min)),
    depth_max = as.numeric(gsub(",", ".", depth_max)))



