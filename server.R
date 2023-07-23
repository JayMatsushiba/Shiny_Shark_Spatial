library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(scales)

tilesURL <- "https://api.mapbox.com/styles/v1/jaymatsushiba/clkfuifsm002e01r1eo1b6sm5/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiamF5bWF0c3VzaGliYSIsImEiOiJjazNtNnZjMjkxY2FnM2ZvMzlteW01YWt3In0.7nVJaIxpI0ZHAm0HHMOnBQ"
accessToken <- "pk.eyJ1IjoiamF5bWF0c3VzaGliYSIsImEiOiJjazNtNnZjMjkxY2FnM2ZvMzlteW01YWt3In0.7nVJaIxpI0ZHAm0HHMOnBQ"

function(input, output, session) {

  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(0, 0, 3) %>%
      # addProviderTiles(providers$CartoDB.Positron) %>%
      addTiles(urlTemplate = tilesURL)
      # remove this later, just was here for troubleshooting attempts to use custom CRS
      # addMarkers(0, 0)
    
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    opacityBy <- input$opacity
    
    
    
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
    
    
    colorData <- areas[[colorBy]]
    opacityData <- areas[[opacityBy]]
    pal <- colorNumeric("viridis", colorData)
    
    if (input$variables == "univariate") {
      leafletProxy("map", data = areas) %>%
        clearShapes() %>%
        addPolygons(data = areas, 
                    stroke = F, 
                    fillColor = pal(colorData),
                    fillOpacity = 0.8,
                    layerId = ~NEW_FID)
    } else {
      leafletProxy("map", data = areas) %>%
        clearShapes() %>%
        addPolygons(data = areas, 
                    stroke = F, 
                    fillColor = pal(colorData),
                    fillOpacity = rescale(opacityData),
                    layerId = ~NEW_FID)
      # addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
      #           layerId="colorLegend")
    }


  })
  
  # Show a popup at the given location
  showAreaPopup <- function(area, lat, lng) {
    selectedArea <- areas[areas$NEW_FID == area,]
    var1 <- input$color
    var2 <- input$opacity
    if (input$variables == "bivariate") {
      content <- as.character(tagList(
        tags$h4("Area:",selectedArea$NEW_FID),
        tags$p(var1, " = ", selectedArea[[var1]]),
        tags$p(var2, " = ", selectedArea[[var2]])
      ))} else {
        content <- as.character(tagList(
          tags$h4("Area:", selectedArea$NEW_FID),
          tags$p(var1, " = ", selectedArea[[var1]])
        ))}
    
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