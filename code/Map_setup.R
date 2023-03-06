## HEADER ####
## Who: GW + PD
## What: loading data for mapping HAU fields and crops
## When: 2023-02-02, last edited 2023-03-03

# Setup -----------------------------------------------------------------
setwd("C:/Users/00758120/Documents/GitHub/haumap02")
source("code/functions/PackageCheck_F.R")
source("code/functions/Standardize.R")

#setwd(r'(C:\Users\00758120\Harper Adams University\George Wager - farm-data)')
# install and load required packages for the session
libs = c("stringr", "openxlsx", "data.table", "readxl", "rgdal", "sf", "raster", "broom", "leaflet", "htmlwidgets")
PackageCheck(libs)

# Location of source files
shpdir <- paste0(getwd(), "/data/harper-farm-boundaries") # shapefiles
croptypes <- read.csv("input/crop_colors.csv") # crop colors

# Make the cropping datafile for the required season ---------------------------
#season <- 2022
source("code/CROPPING.R")
#CropColor <- croptypes$CropColor

# Put the shapefiles together ----------------------------------------------------
file_list <- dir(shpdir, pattern = ".shp", full.names = TRUE)
# read all the shape files and store as a list
shapefile_list <- lapply(file_list, read_sf, as_tibble = T)
# Look to make sure they're all in the same CRS
s <- unique(unlist(sapply(shapefile_list, st_crs)))
cat(s) # exploratory - remove
# Combine the list of sf objects into a single object (rbind.sf())
all_fields <- do.call(what = sf:::rbind.sf, args=shapefile_list)

# matching crops to fields
cr <- MapCropsOntoFields_F(fields1 = all_fields$FIELD_NAME,
                           cropping = cropping,
                           croptypes = croptypes)
all_fields$CROP_TYPE <- cr

source("code/Map_crops.R")
#source("code/Map_fields.R")

# ADD SIMULATED POTATO YIELDS FOR EACH OF THESE LOCATIONS
# meta data of yields from DSSAT predictions
# FIELD 1
# FIELD 2
# FIELD 3

# add the meta data to each unique shapefile

