# haumap v02
# PD
# 2023-03-06 based on earlier code

# Setup -----------------------------------------------------------------
#setwd("C:/Users/00758120/Documents/GitHub/haumap02")
source("code/functions/PackageCheck_F.R")
source("code/functions/Standardize.R")
source("code/functions/Cropping_F.R")
#setwd(r'(C:\Users\00758120\Harper Adams University\George Wager - farm-data)')
# install and load required packages for the session

libs = c("stringr", "openxlsx", "data.table", "readxl", "rgdal", "sf", "raster", "broom", "leaflet", "htmlwidgets")
PackageCheck(libs)

# Location of source files
shpdir <- paste0(getwd(), "/data/harper-farm-boundaries") # shapefiles
croptypes <- read.csv("input/crop_colors.csv") # crop colors

# Put the shapefiles together --------------------------------------------
file_list <- dir(shpdir, pattern = ".shp", full.names = TRUE)
# read all the shape files and store as a list
shapefile_list <- lapply(file_list, read_sf, as_tibble = TRUE)
# Look to make sure they're all in the same CRS
# s <- unique(unlist(sapply(shapefile_list, st_crs)))
# Combine the list of sf objects into a single object (rbind.sf())
all_fields <- do.call(what = sf:::rbind.sf, args=shapefile_list)

# Shiny app -------------------------------------------------------------
# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("HAU Farm crop types"),

    # Sidebar with a select input for the year 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "season", 
                        label = "Choose the harvest year to display on the map",
                        choices = c("2020", 
                                    "2021",
                                    "2022", 
                                    "2023"),
                        selected = "2023"),
            width = 2
        ),

        # Show the map in the main panel
        mainPanel(
          leafletOutput("map", height = "90vh"),
          width = 10
        )
    )
)

# Define server
server <- function(input, output) {
  
      output$map <- renderLeaflet({
        
        cropping <- Cropping_F(season = input$season)
        
        cr <- MapCropsOntoFields_F(fields1 = all_fields$FIELD_NAME,
                                   cropping = cropping,
                                   croptypes = croptypes)
        all_fields$CROP_TYPE <- cr
        
        mycol <- croptypes$CropColor[match(all_fields$CROP_TYPE, croptypes$CropType)]
        
        map <- leaflet()  %>%
          addTiles(group = "Map")  %>%
          addProviderTiles("Esri.WorldImagery", group = "Photo") %>%
          setView(lng = -2.44, lat=52.76,zoom=13) %>%
          addPolygons(data = all_fields, group = all_fields$CROP_TYPE,
                      stroke = T,
                      color = "grey",
                      weight = 1,
                      opacity = 1,
                      fillColor = mycol,
                      fillOpacity = 0.8, 
                      label = ~FIELD_NAME,
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
                    title = "Crop types"
          )

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
