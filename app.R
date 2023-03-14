# haumap v02
# PD
# 2023-03-06 based on earlier code
# last edited 2023-03-14: 
#    - field matching updated based on manually assigned field codes

# Setup -----------------------------------------------------------------
source("code/functions/StandardCropTypes_F.R")
source("code/functions/Cropping_F.R")

library("stringr")
library("sf")
library("leaflet")
library("htmlwidgets")

# Location of source files
shpdir <- paste0("data/harper-farm-boundaries") # shapefiles
croptypes <- read.csv("input/crop_colors.csv") # crop colors
fieldcodes <- read.csv("input/FIELD_CODES.csv") # unique field codes

# Put the shapefiles together --------------------------------------------
file_list <- dir(shpdir, pattern = ".shp", full.names = TRUE)
# read all the shape files and store as a list
shapefile_list <- lapply(file_list, read_sf, as_tibble = TRUE)
all_fields <- do.call(what = sf:::rbind.sf, args=shapefile_list)
colnames(all_fields)[colnames(all_fields)=="FIELD_NAME"] <- "FIELD_NAME_Omnia"
m <- match(all_fields$FIELD_NAME_Omnia, fieldcodes$FIELD_NAME_Omnia)
all_fields$FIELD_CODE <- fieldcodes$FIELD_CODE[m]

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
        colnames(cropping)[colnames(cropping)=="Field"] <- "FIELD_NAME_SumIt"
        m <- match(cropping$FIELD_NAME_SumIt, fieldcodes$FIELD_NAME_SumIt)
        cropping$FIELD_CODE <- fieldcodes$FIELD_CODE[m]
        
        m <- match(all_fields$FIELD_CODE, cropping$FIELD_CODE)
        all_fields$CropType <- cropping$CropType[m]
        all_fields$CropTypeStandard <- StandardCropTypes_F(all_fields$CropType, CropType = croptypes$CropTypeStandard)
        all_fields$Label <- paste0(all_fields$FIELD_NAME_Omnia, " - ", all_fields$CropType)
        
        mycol <- croptypes$CropColor[match(all_fields$CropTypeStandard, croptypes$CropTypeStandard)]
        
        map <- leaflet()  %>%
          addTiles(group = "Map")  %>%
          addProviderTiles("Esri.WorldImagery", group = "Photo") %>%
          setView(lng = -2.44, lat=52.76,zoom=13) %>%
          addPolygons(data = all_fields, group = all_fields$CropTypeStandard,
                      stroke = T,
                      color = "grey",
                      weight = 1,
                      opacity = 1,
                      fillColor = mycol,
                      fillOpacity = 0.8, 
                      label = ~Label,
                      highlightOptions = highlightOptions(color = "#E2068A", weight = 1,
                                                          bringToFront = TRUE, fillOpacity = 0.4)) %>%
          
          addLayersControl(
            baseGroups = c("Map", "Photo"),
            overlayGroups = all_fields$CropTypeStandard,
            options = layersControlOptions(collapsed = F)
          ) %>% 
          addLegend("bottomright",opacity = 1,
                    colors = croptypes$CropColor,
                    labels = croptypes$CropTypeStandard,
                    title = "Crop types"
          )

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
