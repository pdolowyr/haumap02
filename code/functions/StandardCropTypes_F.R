# Standardize Crop Type names

StandardCropTypes_F <- function(s, CropType){
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
  s[!(s %in% CropType)] <- "other"
  return(s)
}