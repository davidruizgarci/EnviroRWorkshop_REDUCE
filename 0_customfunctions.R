#-------------------------------------------------------------------------------
#
# Title: Custom functions
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Authors: David Ruiz-García
# Last revision: 2025/03/13
#
#-------------------------------------------------------------------------------

# Extract the three 3D possibilities at once (surfece, bottom and nearest)
cmems3d_all <- function(lon, lat, date, productid, repo, data, id, maxZ = NULL) {
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # data        Your database
  # lon         longitude
  # lat         latitude
  # date        POSIXct date time or Date
  # id          identificator of each observation/position/tow
  # depth       depth value, in meters (positive)
  # productid   id of the product from catalog table. This is used to find the netcdf file from the repository (repo)
  # repo        repository path with netcdf files. it follows the same structure as the CMEMS FTP server
  # maxZ        optional. Maximum Z level to extract the data from. ROMS has 35 levels. (level 23=300m)
  
  # Value
  # A data frame with: varname, var0(optional), zmax(optional)
  
  # Description
  # Extraction of values is done using the nearest neighbor 3d point.
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$var)
  
  # Extract unique idns
  idns <- as.data.frame(unique(id))
  colnames(idns) <- "idns"
  
  # Initialize the results matrix
  max_depth_levels <- 48  # This should be the maximum depth levels based on your data
  results <- matrix(data = NA, nrow = max_depth_levels, ncol = nrow(idns), byrow = FALSE,  
                    dimnames = list(1:max_depth_levels, idns$idns))
  
  # Initialize vectors to store surface, bottom, and nearest depth data
  unique_surface <- vector("numeric", length(data$date))
  unique_bottom <- vector("numeric", length(data$date))
  unique_nearest <- vector("numeric", length(data$date))
  
  # get data for each observation
  for (i in 1:length(date)) {
    #i=1
    print(i)
    
    # get day, lon, lat, depth
    iday <- as.POSIXct(data$date[i], format = "%Y-%m-%d", tz = "UTC")
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    
    # create direction
    date <- as.character(iday)
    year = format(iday, "%Y")
    month = format(iday, "%m")
    day = format(iday, "%d")
    ncdir <- paste(repo, year, month, day, sep="/")
    
    # open netcdf matching(d)
    product_files <- list.files(paste(ncdir), pattern = "_3D_.*\\.nc$", full.names=TRUE, recursive=TRUE)
    if (length(product_files) == 0) {
      print("No product files found in the specified repository path.")
      next  # <<<< Skips to the next iteration of the loop
    }
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    # select first file and get dimensions
    nclon <- nc$dim$lon$vals 
    nclat <- nc$dim$lat$vals 
    ncdepth <- nc$dim$depth$vals
    maxZ <- if (is.null(maxZ)) nc$dim$depth$len else maxZ
    nctime <- nc$dim$time$vals
    ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC")
    
    # identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon))
    minlat <- which.min(abs(nclat - ilat))
    mintime <- which.min(abs(ncday - iday))
    mindepth <- which.min(abs(ncdepth - idepth))
    
    
    # get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1))
    
    # close nc
    nc_close(nc)
    
    # Find the column index in results that corresponds to the current id
    #idn <- idns[1]
    col_idx <- which(idns$idns == id)
    results[1:maxZ, col_idx] <- ncdata
    
    # Surface Data: Data from the first depth level
    unique_surface[i] <- ncdata[1]
    
    # Bottom Data: Data from the last depth level
    last_non_na_idx <- max(which(!is.na(ncdata)))
    unique_bottom[i] <- if (length(last_non_na_idx) > 0) ncdata[last_non_na_idx] else NA
    
    # Nearest Depth Data: Data closest to the target depth
    min_depth_diff <- which.min(abs(ncdepth - idepth))
    unique_nearest[i] <- ncdata[min_depth_diff]
    
  }
  
  # Add the data to your dataframe
  data[[paste0("seasurface_", product_info$variable, "_", product_info$product_type)]] <- unique_surface
  data[[paste0("seabottom_", product_info$variable, "_", product_info$product_type)]] <- unique_bottom
  data[[paste0("nearest_", product_info$variable, "_", product_info$product_type)]] <- unique_nearest
  
  return(data)
}

