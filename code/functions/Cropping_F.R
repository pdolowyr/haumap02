
# Cropping_F
# To be called from Map_setup
# slim down by removing exploratory code
# PD
# last edited 2023-03-06

Cropping_F <- function(season){
  # Read data in --------------------------
  X <- read.csv("data/arable_usage_full_2022_01_18.csv")
  # restrict the cropping info from Sumit to no earlier than
  X$Date <- as.Date.character(X$Date, format = c("%d/%m/%Y"))
  X <- X[X$Date >= "2020-01-01",] # earliest date
  
  # Findings from data exploration -------------------------
  X <- X[, -which(colnames(X)=="Action")]
  X <- X[, -which(colnames(X)=="TotalQty")]
  X$QtyPerOperation <- X$Area_Hctrs * X$QtyPerHctr
  X <- X[, -which(colnames(X)=="Total")]
  X$CostPerOperation <- X$Area_Hctrs * X$CostPerHctr
  cln <- colnames(X)
  cln <- str_replace_all(cln, "Hctrs", "Ha")
  cln <- str_replace_all(cln, "Hctr", "Ha")
  cln <- str_replace_all(cln, "UoM", "UnitsOfQty")
  colnames(X) <- cln
  
  # correct unit abbreviations -------------
  U <- X$UnitsOfQty
  U <- str_replace_all(U, "Hctr", "")
  U <- str_replace_all(U, "Kgs", "kg")
  U <- str_replace_all(U, "Litres", "litre")
  U <- str_replace_all(U, "Gms", "g")
  U <- str_replace_all(U, "Tonne", "t")
  U <- str_replace_all(U, "Unit", "")
  U <- str_replace_all(U, "Bales", "Bale")
  X$UnitsOfQty <- U
  
  X$Category[tolower(X$Description)==tolower("Harvested (contractor)")] <- "HARVESTED"
  X$Category[tolower(X$Description)==tolower("Combine")] <- "HARVESTED"
  X$Category[tolower(X$Description)==tolower("Combine (contractor)")] <- "HARVESTED"
  X$Category[tolower(X$Description)==tolower("Crop Season Start Marker")] <- "CROP_IN"
  X$Category[tolower(X$Description)==tolower("Drilled")] <- "CROP_IN"
  X$Category[tolower(X$Category)==tolower("Seeds")] <- "CROP_IN"
  X <- X[order(X$Field, X$Date),]
  
  # what crop on field a at date d?
  d <- paste0(season, "-06-30")
  u <- unique(X$Field)
  lcr_date <- as.Date(character(length(u)))
  lcr_type <- character(length(u))
  hrv_date <- as.Date(character(length(u)))
  hrv_yield <- numeric(length(u))
  for(i in 1:length(u)){
    d0 <- X$Date[X$Field==u[i] & X$Category=="CROP_IN" & X$Date<=d]
    if(length(d0)>0) {
      lcr_date[i] <- max(d0)
      cr <- X$Crop[X$Field==u[i] & X$Category=="CROP_IN" & X$Date==lcr_date[i]]
      if(!all(cr=="")){
        cr <- cr[cr!=""]
      } else {
        cr <- cr[1]
      }
      lcr_type[i] <- cr
    } else {
      lcr_date[i] <- NA
      lcr_type[i] <- NA
    }
    # when was the crop in question harvested?
    d0 <- X$Date[X$Field==u[i] & X$Category=="HARVESTED" & X$Date>=d]
    if(length(d0)>0) {
      hrv_date[i] <- min(d0)
      hr <- X$QtyPerHa[X$Field==u[i] & X$Category=="HARVESTED" &X$Date==hrv_date[i]]
      if(!all(hr=="")){
        hr <- hr[hr!=""]
      } else {
        hr <- hr[1]
      }
      hrv_yield[i] <- hr
    } else {
      hrv_date[i] <- NA
      hrv_yield[i] <- NA
    }
  }
  cropping <- data.frame("Field" = u,
                         "CutoffDate" = d,
                         "CropInDate" = lcr_date,
                         "CropType" = lcr_type,
                         "HarvestDate" = hrv_date,
                         "Yield" = hrv_yield)
  return(cropping)
}
