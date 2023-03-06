# Standardize field names and crop names
# PD
# 2023 03 03

# croptypes
# fields
# cropping

StandardFieldName_F <- function(s){
  s <- tolower(s)
  s <- str_remove_all(s, "'")
  s <- str_replace_all(s, "dog leg", "dogleg")
  s <- str_replace_all(s, "brids nest", "birdsnest")
  s <- str_replace_all(s, "black britch", "blackbritch")
  s <- str_replace_all(s, "large marsh", "largemarsh")
  s <- str_replace_all(s, "near moor", "nearmoor")
  s <- str_replace_all(s, "broard", "broad")
  s <- str_replace_all(s, "near broad", "nearbroad")
  
  return(s)
}

StandardCropTypes_F <- function(s, croptypes){
  s <- tolower(s)
  s[is.na(s) | s==""] <- "unknown"
  s <- str_remove_all(s, "winter")
  s <- str_remove_all(s, "spring")
  s <- str_remove_all(s, "oilseed")
  s <- str_replace_all(s, "grassland", "grass")
  s <- str_replace_all(s, "red clover", "clover")
  s <- str_replace_all(s, "white clover", "clover")
  s <- str_trim(s, "both")
  s <- str_replace_all(s, "rape", "osr")
  s <- str_replace_all(s, "oats", "oat")
  s[!(s %in% croptypes$CropType)] <- "other"
  return(s)
}

MapCropsOntoFields_F <- function(fields1, cropping, ...){
  # standardize field names
  fi1 <- StandardFieldName_F(fields1) # source of shapefiles
  fi2 <- StandardFieldName_F(cropping$Field) # info on cropping
 # match field names
  ifields <- match(fi1, fi2) # indexing the rows in the cropping table based on standardized field names
  cr <- cropping$CropType[ifields] # standardized crop types mapped onto the field list (=onto the spatial object)
  # standardize crop names
  cr <- StandardCropTypes_F(cr, croptypes = ...) #CropType = croptypes$CropType
  return(cr)
}

