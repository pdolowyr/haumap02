# to be run from Map_setup

# Map of crop types -----------------------------------------------------------------
# matching crops to fields

mycol <- croptypes$CropColor[match(all_fields$CROP_TYPE, croptypes$CropType)]
map <- leaflet()  %>%
  addTiles(group = "Map")  %>%
  # addProviderTiles("Esri.WorldImagery", group = "Photo") %>%
  setView(lng = -2.44, lat=52.76,zoom=13) %>%
  addPolygons(data = all_fields, group = all_fields$CROP_TYPE,
              stroke = T,
              color = "grey",
              weight = 1,
              opacity = 1,
              fillColor = mycol,
              fillOpacity = 0.8, 
              label = ~FIELD_NAME,
              #             popup = all_fields$FIELD_NAME,
              highlightOptions = highlightOptions(color = "#E2068A", weight = 1,
                                                  bringToFront = TRUE, fillOpacity = 0.4)) %>%
  
  addLayersControl(
    baseGroups = c("Map", "Photo"),
    overlayGroups = all_fields$CROP_TYPE,
    options = layersControlOptions(collapsed = F)
  ) %>% 
  addLegend("bottomright",opacity = 1,
            colors = croptypes$CropColor,
            labels = croptypes$CropType,
            title = "Crop types on HAU fields \n(2021 harvest season)"
  )
map
# Save the widget as html ------------------------------------------------------
# htmlwidgets::saveWidget(widget = map, 
#                         file=paste0( getwd(), "/maps/HAU_Crop_types.html"),
#                         title = "HAU Crop types")
