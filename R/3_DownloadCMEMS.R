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

# 1.Prepare your dataset--------------------------------------------------------

# 1.1. Open your dataset:
data <- read.csv2("R/input/dataset.csv", sep = ";")
View(data)
head(data)

# How many dates do we have?
# Response:


# Plot data to have a reference:
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

# Load land mask:
world <- ne_countries(scale = "medium", returnclass = "sf")
# Make plot:

# Zoom in:
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +
  geom_point(data = data, aes(x = lon, y = lat, color = depth), size = 3) +
  scale_color_viridis_c(name = "Depth (m)", option = "D", direction = -1) +
  coord_sf(xlim = range(data$lon) + c(-1, 1), ylim = range(data$lat) + c(-1, 1), expand = FALSE) +
  theme_minimal() +
  labs(title = "Observation Locations within Cabo Verde",
       x = "Longitude", y = "Latitude")

# Zoom out:
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +
  geom_point(data = data, aes(x = lon, y = lat, color = depth), size = 3) +
  scale_color_viridis_c(name = "Depth (m)", option = "D", direction = -1) +
  coord_sf(xlim = range(data$lon) + c(-10, 15), ylim = range(data$lat) + c(-10, 10), expand = FALSE) +
  theme_minimal() +
  labs(title = "Observation Locations within Cabo Verde",
       x = "Longitude", y = "Latitude")


# 1.2. Make a dataset with your dates:
# We are going to work with a daily resolution, then we want to adjust the download to those days in which you have data. 
# Set date format as "date" (year-moth-day)
library(dplyr)
data$date <- as.Date(data$date)
# Select uniqued dates
Days <- unique(data$date)
# Make a dataframe
Days_df <- data.frame(Days)
# Ensure date format in the new dataframe
Days_df$Days <- as.Date(Days_df$Days)
# check it out:
head(Days_df) #2024-12-25

# We may want to have a year, month and day columns to later organise the downloading of files
# Add a new column with the year information
Days_df <- Days_df %>%
  mutate(Year = format(Days, "%Y"),
         Month = format(Days, "%m"),
         Day = format(Days, "%d"))
head(Days_df)

# add mins and secs (as we are working with daily resolution we dont need the particular time of each observation):
# Note: 11:00:00 if you use 12:00:00 CMEMS use the next day!
Days_df$Days_with_time <- paste0(Days_df$Days, " 11:00:00")
head(Days_df)


# 2. Prepare your catalog------------------------------------------------------

# 2.1. Import data catalog
# Remember, the catalog is where you have the required information for download
catalog <- read.csv2("R/input/Catalog_CMEMS.csv", sep=";")
# Check it out and ensure numerical variables are numeric
str(catalog) 

# Convert to numerical if they aren't:
catalog <- catalog %>%
  mutate(
    xmin = as.numeric(gsub(",", ".", xmin)),
    xmax = as.numeric(gsub(",", ".", xmax)),
    ymin = as.numeric(gsub(",", ".", ymin)),
    ymax = as.numeric(gsub(",", ".", ymax)),
    depth_min = as.numeric(gsub(",", ".", depth_min)),
    depth_max = as.numeric(gsub(",", ".", depth_max)))
str(catalog)


# 3. Log-in in CMEMS through Command Line Interface (CLI) ----------------------
# For more info: https://help.marine.copernicus.eu/en/articles/8638253-how-to-download-data-via-the-copernicus-marine-toolbox-in-r

# 3.1. Install python (you should have done this in the package installing part)
library(reticulate)
#install_python() 
# 3.2. Create an environment (you should have done this in the package installing part)
#virtualenv_create(envname = "cmems")
#virtualenv_install("cmems", packages = c("copernicusmarine"))

# 3.3. Load into your environment
# To avoid writing your user and password here (visible to anyone with access) keep it a txt file
path <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/user.txt"
#if(cpu == "yours") path <- "..."
username <- paste(readLines(path, warn = FALSE), collapse = "")

path <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/psw.txt"
#if(cpu == "yours") path <- "..."
password <- paste(readLines(path, warn = FALSE), collapse = "")


use_virtualenv("cmems", required = TRUE)
cm <- import("copernicusmarine")

# Log in in your CMEMS user:
cm$login(username, password)
# for session yes == y
y


# 4. Download CMEMS data -------------------------------------------------------
# 4.1. Download a single file:
# Let's start with the first product on the first date:
cat <- catalog %>%
  filter(id_product  %in% c("1")) 

# Required information for download:
dataset_id <- cat$dataset_id
print(dataset_id)

start_datetime <- min(Days_df$Days_with_time)
print(start_datetime)

end_datetime <- max(Days_df$Days_with_time)
print(end_datetime)

variables <- list(cat$var) # attention - variables must be a list
print(variables)

minimum_longitude <- cat$xmin
print(minimum_longitude)

maximum_longitude <-  cat$xmax
print(maximum_longitude)

minimum_latitude <-  cat$ymin
print(minimum_latitude)

maximum_latitude <- cat$ymax
print(maximum_latitude)

minimum_depth <- cat$depth_min
print(minimum_depth)

maximum_depth <- cat$depth_max
print(maximum_depth)

# Naming the file:
output_filename <- paste0(cat$var_name, "_", Days_df$Days, ".nc")
print(output_filename)

# Selecting where to save it:
# Generate a folder within input
destination_folder <- paste0("R/input/cmems")
if (!dir.exists(destination_folder)) dir.create(destination_folder, recursive = TRUE)
print(destination_folder)

# Generate a folder for the product
output_directory <- file.path(destination_folder, Days_df$Year, Days_df$Month, Days_df$Day)
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
print(output_directory)

# Download:
cm$subset(dataset_id = dataset_id,
          start_datetime = start_datetime,
          end_datetime = end_datetime,
          variables = variables,
          minimum_longitude = minimum_longitude,
          maximum_longitude = maximum_longitude,
          minimum_latitude = minimum_latitude,
          maximum_latitude = maximum_latitude,
          minimum_depth = minimum_depth,
          maximum_depth = maximum_depth,
          output_filename = output_filename,
          output_directory = output_directory)

# Check the file:
library(ncdf4)
library(raster)

# 2D netCDF files:
path <- paste0(output_directory, "/", output_filename)
nc <- nc_open(path)
nc

# extract data to check it out:
lon <- nc$dim$lon$vals
print(lon)

lat <- nc$dim$lat$vals
print(lat)

depth <- nc$dim$depth$vals
print(depth)

time <- nc$dim$time$vals
print(time)

# Convert format
reference_date <- as.POSIXct("1950-01-01 00:00:00", tz = "UTC")
# Convert hours to seconds and add to reference date
time_converted <- reference_date + time * 3600  # 1 hour = 3600 seconds
print(time_converted)

# Calculate the resolution in latitude and longitude
lat_resolution <- abs(lat[2] - lat[1])
lon_resolution <- abs(lon[2] - lon[1])
print(lat_resolution)
print(lon_resolution)



# 4.2. Download a group of variables and/or dates-------------------------------
# Subset dates and products if you wish:
# Define the time subset you want:
df <- Days_df 
head(df)

# Define the catalog subset you want:
cat <- catalog
head(cat)
#cat <- catalog %>%
#  filter(dimensions %in% c("2D")) 

# Create folder where you are going to save to files:
destination_folder <- paste0("R/input/cmems")
if (!dir.exists(destination_folder)) dir.create(destination_folder, recursive = TRUE)

t <- Sys.time()
for(i in 1:nrow(cat)){ 
  
  # Calculate remaining products
  #i=2
  remaining_products <- nrow(cat) - i
  
  #If you need a folder per each date:
  #j=1
  for(j in 1:nrow(df)){
    # Calculate remaining dates
    remaining_dates <- nrow(df) - j
    
    # Print the current product and remaining products
    print(paste("Processing product", i, "of", nrow(cat), "-", remaining_products, "remaining"))
    # Print the current date and remaining dates
    print(paste("Processing date", j, "of", nrow(df), "-", remaining_dates, "remaining"))
    
    
    # Create folders for different dates inside the variable folders
    date_dir <- file.path(destination_folder, df$Year[j], df$Month[j], df$Day[j])
    if (!file.exists(date_dir)) {
      dir.create(date_dir, recursive = TRUE)}
    
    # Define the file name using the current date
    file_name <- paste0(cat$var_name[i], "_", df$Days[j], ".nc")
    
    # download data
    cm$subset(
      dataset_id = cat$dataset_id[i],
      start_datetime = df$Days_with_time[j], #format example "1994-05-16 12:00:00"
      end_datetime = df$Days_with_time[j],
      variables = list(cat$var[i]), # attention - variable must be a list
      minimum_longitude = cat$xmin[i],
      maximum_longitude =  cat$xmax[i],
      minimum_latitude =  cat$ymin[i],
      maximum_latitude = cat$ymax[i],
      minimum_depth = cat$depth_min[i],
      maximum_depth = cat$depth_max[i],
      output_filename = file_name,
      output_directory = date_dir)
  }
}
Sys.time() - t 


# Check the 3D file:
path <- paste0(output_directory, "/CHL_Analysis_3D_2024-12-25.nc")
nc <- nc_open(path)
nc

# extract data to check it out:
lon <- nc$dim$lon$vals
print(lon)

lat <- nc$dim$lat$vals
print(lat)

depth <- nc$dim$depth$vals
print(depth)

time <- nc$dim$time$vals
print(time)

# Convert format
reference_date <- as.POSIXct("1950-01-01 00:00:00", tz = "UTC")
# Convert hours to seconds and add to reference date
time_converted <- reference_date + time * 3600  # 1 hour = 3600 seconds
print(time_converted)

# Calculate the resolution in latitude and longitude
lat_resolution <- abs(lat[2] - lat[1])
lon_resolution <- abs(lon[2] - lon[1])
print(lat_resolution)
print(lon_resolution)
