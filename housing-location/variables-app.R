
library(leaflet)
library(sf)
library(shiny)
library(tmap)

# Get data
load("data.RData")

# Define data type
polygonData <- c("sf.parks", "sf.waterfront", "sf.airports")
pointData <- c("sf.playground", "sf.highschools", "sf.convenience", "sf.supermarket", 
               "sf.shopping", "sf.fitness", "sf.cinema", "sf.bars", "sf.cafes",
               "sf.restaurants", "sf.noise", "sf.subway", "sf.crime", "sf.elementary")
lineData <- c("sf.trunkroad", "sf.mainroad", "sf.motorway_link", "sf.motorway", 
              "sf.railways")

# Set user interface
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("tmap", width = "100%", height = "100%"),
  absolutePanel(top = 20, right = 20,
                
                # Define a range
                # sliderInput("range", "Price per sqm", round(min(sf.nyc$price_m2),0), round(max(sf.nyc$price_m2),0),
                #            value = range(sf.nyc$price_m2), step = 100
                # ),
                
                # Variable selection
                radioButtons(inputId = "variable", label = "Choose a variable", choices = 
                               c("Waterfront access" = "sf.waterfront",
                                 "Playgrounds" = "sf.playground",
                                 "High schools" = "sf.highschools",
                                 "Elementary schools" = "sf.elementary",
                                 "Shootings" = "sf.crime",
                                 "Subway stations" = "sf.subway",
                                 "Restaurants" = "sf.restaurants",
                                 "Cafes" = "sf.cafes",
                                 "Bars" = "sf.bars",
                                 "Cinemas" = "sf.cinema",
                                 "Airports" = "sf.airports",
                                 "Fitness centres" = "sf.fitness",
                                 "Railways" = "sf.railways",
                                 "Shopping centres" = "sf.shopping",
                                 "Supermarkets" = "sf.supermarket",
                                 "Convenience stores" = "sf.convenience",
                                 "Parks" = "sf.parks",
                                 "Motorway" = "sf.motorway",
                                 "Motorway links" = "sf.motorway_link",
                                 "Main roads" = "sf.mainroad",
                                 "Trunk roads" = "sf.trunkroad"))
  )
)

# Set server
server <- function(input, output, session) {
  
  # Map creation
  output$tmap = renderLeaflet({

    if (input$variable %in% polygonData) {
      tmap <- tm_shape(sf.boroughs) +
        tm_polygons(alpha = 0.1, border.alpha = 0.4) +
        tm_shape(get(input$variable)) +
        tm_polygons(col = "blue", alpha = 0.4, border.alpha = 0.8)
    } else if (input$variable %in% lineData) {
      tmap <- tm_shape(sf.boroughs) +
        tm_polygons(alpha = 0.1, border.alpha = 0.4) +
        tm_shape(get(input$variable)) +
        tm_lines(lwd = 1, col = "blue")
    } else {
      tmap <- tm_shape(sf.boroughs) +
        tm_polygons(alpha = 0.1, border.alpha = 0.4) +
        tm_shape(get(input$variable)) +
        tm_dots(size = 0.05, col = "blue", alpha = 1)
    }
    
    tmap_leaflet(tmap)
  })
  
}

# Run shiny app
shinyApp(ui, server)