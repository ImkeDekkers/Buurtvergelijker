icongreen <- makeAwesomeIcon(
  icon = "arrow-down",
  iconColor = "black",
  markerColor = "green",
  library = "fa")

# Function that makes map of the selected variable 
make_map <- function(dataset, variable, input_niveau_facilities, theme){
  #get input for the map
  map_data <- dataset
  map_data$variable <- map_data[[variable]]
  
  # Define colors for polygons and legend 
  pal <- colorBin("YlOrRd", domain = map_data$variable)
  qpal <- colorQuantile("YlOrRd", map_data$variable, n = 6)
  # For the colors in the map, colorQuantile is used, unless an error is given, then we use colorBin
  coloring <- tryCatch({
    qpal(map_data$variable)
  } , error = function(e) {
    pal(map_data$variable)
  } )
  legend_title <- as.character(variable)
  labels <- sprintf("%s: %g", map_data$NAAM, map_data$variable) %>% 
    lapply(htmltools::HTML)
  label_content <- sprintf("%s: %g <br/> %s: %g", 
                           map_data$selected_area_label, map_data$variable[map_data$CODE == map_data$selected_area_code], "Gemiddelde vergelijkbare gebieden", round(mean(map_data$variable, na.rm=TRUE), digits = 1))%>% lapply(htmltools::HTML)
  
  #selected_area_code <- df[1, "selected_area_code"]
  
  # Map
  output_map <- tryCatch({
    leaflet(map_data)%>%
      addPolygons(fillColor = ~coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addMarkers(
        lng = map_data$centroidxx, lat = map_data$centroidyy,
        label = label_content,
        labelOptions = labelOptions(noHide = T))%>%
      addAwesomeMarkers(data = top5_distances_theme(dataset, input_niveau_facilities, theme),
                        lng = ~centroidx,
                        lat = ~centroidy,
                        icon = icongreen,
                        label = ~NAAM) %>% 
      #addCircleMarkers(lng = map_data$centroidxx, lat = map_data$centroidyy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
      leaflet::addLegend(pal = qpal, values = ~map_data$variable, opacity = 0.7, title = legend_title, position = "bottomright", labFormat = function(type, cuts, p) {      #labformat function makes sure the actual values instead of the quantiles are displayed in the legend
        n = length(cuts)
        paste0(round(cuts[-n],2), " &ndash; ", round(cuts[-1],2))
      }#, labFormat = labelFormat(digits = 0)
      )
    
  }, error = function(e) {
    leaflet(map_data) %>%
      addPolygons(fillColor = ~ coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addMarkers(
        lng = map_data$centroidxx, lat = map_data$centroidyy,
        label = label_content,
        labelOptions = labelOptions(noHide = T))%>% 
      addAwesomeMarkers(data = top5_distances_theme(dataset, input_niveau_facilities, theme),
                        lng = ~centroidx,
                        lat = ~centroidy,
                        icon = icongreen,
                        label = ~NAAM) %>% 
      #addCircleMarkers(lng = map_data$centroidx, lat = map_data$centroidy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
      leaflet::addLegend(pal = pal, values = ~map_data$variable, opacity = 0.7, title = legend_title, position = "bottomright")
  })
  
  return(output_map)
  
}
