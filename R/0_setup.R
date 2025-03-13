#-------------------------------------------------------------------------------
#
# Title: Setup project directories
# Course: Environmental Data Extraction and Analysis from Satellite Telemetry and Other Sources
#
# Authors: Sarah Saldanha, David March, David Ruiz-García
# Last revision: 2025/03/13
#
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# setup.R         Setup project
#--------------------------------------------------------------------------------

# 1. Set main data paths--------------------------------------------------------
cpu <- "david"  
#cpu <- "yours"

# 1.1. Write the path to the folder where you have your data:
if(cpu == "david") main_dir <- "C:/Users/david/OneDrive/Escritorio/EnviroRWorkshop_REDUCE"
#if(cpu == "yours") main_dir <- "yours"

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
if(cpu == "david") path <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/user.txt"
#if(cpu == "yours") path <- "..."
username <- paste(readLines(path, warn = FALSE), collapse = "")

if(cpu == "david") path <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/psw.txt"
#if(cpu == "yours") path <- "..."
password <- paste(readLines(path, warn = FALSE), collapse = "")

