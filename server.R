library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

function(input, output, session) {

  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(0, 0, 3) %>%
      addTiles() %>%
      # remove this later, just was here for troubleshooting attempts to use custom CRS
      addMarkers(0, 0)
    
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    
    
    # if (colorBy == "superzip") {
    #   # Color and palette are treated specially in the "superzip" case, because
    #   # the values are categorical instead of continuous.
    #   colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
    #   pal <- colorFactor("viridis", colorData)
    # } else {
    #   colorData <- zipdata[[colorBy]]
    #   pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    # }
    # 
    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
    #   radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    # }
    
    leafletProxy("map", data = hexagons) %>%
      clearShapes() %>%
      addPolygons(data = hexagons, 
                  stroke = ~colorQuantile("YlOrRd", Freq)(Freq), 
                  fillColor = ~colorQuantile("YlOrRd", Freq)(Freq),
                  fillOpacity = 0.7,
                  layerId = ~NEW_FID)
      # addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
      #           layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showAreaPopup <- function(area, lat, lng) {
    # selectedZip <- allzips[allzips$zipcode == zipcode,]
    selectedArea <- hexagons[hexagons$NEW_FID == area,]
    content <- as.character(tagList(
      tags$h4("Species Richness:", as.integer(selectedArea$Freq)),
      # tags$strong(HTML(sprintf("%s, %s %s",
      #                          selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      # ))), tags$br(),
      # sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      # sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      # sprintf("Adult population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = area)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    print(input$map_shape_click)
    if (is.null(event))
      return()
    
    isolate({
      showAreaPopup(event$id, event$lat, event$lng)
    })
  })
  
  
  
}