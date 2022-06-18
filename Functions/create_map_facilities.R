# Icons for leaflet maps for different purposes
iconblue <- makeAwesomeIcon(
  icon = "arrow-down",
  iconColor = "black",
  markerColor = "blue",
  library = "fa")

iconred <- makeAwesomeIcon(
  icon = "arrow-down",
  iconColor = "black",
  markerColor = "red",
  library = "fa")

icongreen <- makeAwesomeIcon(
  icon = "arrow-down",
  iconColor = "black",
  markerColor = "green",
  library = "fa")

# Leaflet for prime map of selected area and general comparable areas
prime_map_facilities <- function(dataset){
  leaflet(dataset) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(color = "navy", weight = 1, 
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 2),
              label = ~htmlEscape(dataset$NAAM)) %>% 
  addAwesomeMarkers(lng = dataset$centroidxx,
                    lat = dataset$centroidyy,
                    icon = iconblue) %>% 
  addAwesomeMarkers(data = top5_distances_overall(dataset),
                    lng = ~centroidx,
                    lat = ~centroidy,
                    icon = iconred,
                    label = ~NAAM)}

# Function that makes map of the selected variable 
make_map <- function(dataset, variable, input_niveau_facilities, theme){
  # Get input for the map
  map_data <- dataset
  map_data$variable <- map_data[[variable]]
  
  # Define colors for polygons and legend 
  pal <- colorBin("YlOrRd", domain = map_data$variable)
  qpal <- colorQuantile("YlOrRd", map_data$variable, n = 6)
  
  # For the colors in the map, colorQuantile is used, unless an error is given, then we use colorBin
  coloring <- tryCatch({
    qpal(map_data$variable)
  }, error = function(e) {
    pal(map_data$variable)
  })
  
  legend_title <- as.character(variable)
  legend_title <- paste(strwrap(legend_title, 20), collapse = "<br/>") %>%
    lapply(htmltools::HTML)
  labels <- sprintf("%s: %g", map_data$NAAM, map_data$variable) %>% 
    lapply(htmltools::HTML)
  label_content <- sprintf("%s: %g <br/> %s: %g", 
                           map_data$selected_area_label, 
                           map_data$variable[map_data$CODE == map_data$selected_area_code], 
                           "Gemiddelde vergelijkbare gebieden", round(mean(map_data$variable, na.rm = TRUE), 
                                                                      digits = 1)) %>% 
    lapply(htmltools::HTML)
  
  # Map
  output_map <- tryCatch({
    leaflet(map_data) %>%
      addPolygons(fillColor = ~coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = 'white', weight = 0.5, fillOpacity = 0.7, bringToFront = TRUE), 
                  label = labels) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        lng = map_data$centroidxx, 
        lat = map_data$centroidyy,
        label = label_content,
        labelOptions = labelOptions(noHide = T)) %>%
      addAwesomeMarkers(data = top5_distances_theme(dataset, input_niveau_facilities, theme),
                        lng = ~centroidx,
                        lat = ~centroidy,
                        icon = icongreen,
                        label = ~NAAM) %>% 
      leaflet::addLegend(pal = qpal, values = ~map_data$variable, opacity = 0.7, 
                         title = legend_title, position = "bottomright", 
                         labFormat = function(type, cuts, p) {                    #labformat function makes sure the actual values instead of the quantiles are displayed in the legend
        n = length(cuts)
        paste0(round(cuts[-n],2), " &ndash; ", round(cuts[-1],2))
      }
      )
    
  }, error = function(e) {
    leaflet(map_data) %>%
      addPolygons(fillColor = ~coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = 'white', weight = 0.5, 
                                                      fillOpacity = 0.7, bringToFront = TRUE), 
                  label = labels) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addMarkers(
        lng = map_data$centroidxx, 
        lat = map_data$centroidyy,
        label = label_content,
        labelOptions = labelOptions(noHide = T)) %>% 
      addAwesomeMarkers(data = top5_distances_theme(dataset, input_niveau_facilities, theme),
                        lng = ~centroidx,
                        lat = ~centroidy,
                        icon = icongreen,
                        label = ~NAAM) %>% 
      leaflet::addLegend(pal = pal, values = ~map_data$variable, opacity = 0.7, 
                         title = legend_title, position = "bottomright")
  })
  
  return(output_map)
  
}


# Maps for all variables with distance to closest spot
map_subtheme <- function(data, subthema, niveau, thema){
  dataset <- data
  if (subthema == "Huisartsenpraktijk"){
    make_map(dataset, "Afstand tot huisartsenpraktijk (km)",
             niveau, 
             thema)
  }else if (subthema == "Ziekenhuis incl. buitenpolikliniek"){
    make_map(dataset, "Afstand tot ziekenhuis incl. buitenpolikliniek (km)",
             niveau, 
             thema)
  }else if (subthema == "Ziekenhuis excl. buitenpolikliniek"){
    make_map(dataset, "Afstand tot ziekenhuis excl. Buitenpolikliniek (km)",
             niveau, 
             thema)
  }else if(subthema=="Apotheek"){
    make_map(dataset, "Afstand tot apotheek (km)",
             niveau, 
             thema)
  }else if (subthema == "Supermarkt"){
    make_map(dataset, "Afstand tot grote supermarkt (km)",
             niveau, 
             thema)
  }else if (subthema == "Overige dagelijkse levensmiddelen"){
    make_map(dataset, "Afstand tot overige dagelijkse levensmiddelen (km)",
             niveau, 
             thema)
  }else if (subthema == "Warenhuis"){
    make_map(dataset, "Afstand tot warenhuis (km)",
             niveau, 
             thema)
  }else if (subthema == "Cafe"){
    make_map(dataset, "Afstand tot cafe (km)",
             niveau, 
             thema)
  }else if (subthema == "Cafetaria"){
    make_map(dataset, "Afstand tot cafetaria (km)",
             niveau, 
             thema)
  }else if (subthema == "Restaurant"){
    make_map(dataset, "Afstand tot restaurant (km)",
             niveau, 
             thema)
  }else if (subthema == "Hotel"){
    make_map(dataset, "Afstand tot hotel (km)",
             niveau, 
             thema)
  }else if (subthema == "Kinderdagverblijf"){
    make_map(dataset, "Afstand tot kinderdagverblijf  (km)",
             niveau, 
             thema)
  }else if (subthema == "Buitenschoolse opvang"){
    make_map(dataset, "Afstand tot buitenschoolse opvang  (km)",
             niveau, 
             thema)
  }else if (subthema == "Basisschool"){
    make_map(dataset, "Afstand tot basisscholen (km)",
             niveau, 
             thema)
  }else if (subthema == "Voortgezet onderwijs"){
    make_map(dataset, "Afstand tot voortgezet onderwijs (km)",
             niveau, 
             thema)
  }else if (subthema == "VMBO school"){
    make_map(dataset, "Afstand tot scholen VMBO (km)",
             niveau, 
             thema)
  }else if (subthema == "HAVO/VWO school"){
    make_map(dataset, "Afstand tot scholen HAVO/VWO (km)",
             niveau, 
             thema)
  }else if (subthema == "Oprit hoofdverkeersweg"){
    make_map(dataset, "Afstand tot oprit hoofdverkeersweg (km)",
             niveau, 
             thema)
  }else if (subthema == "Treinstation"){
    make_map(dataset, "Afstand tot treinstation (km)",
             niveau, 
             thema)
  }else if (subthema == "Belangrijk overstapstation"){
    make_map(dataset, "Afstand tot belangrijk overstapstation (km)",
             niveau, 
             thema)
  }else if (subthema == "Bioscoop"){
    make_map(dataset, "Afstand tot bioscoop (km)",
             niveau, 
             thema)
  }else if (subthema == "Attractie"){
    make_map(dataset, "Afstand tot attractie (km)",
             niveau, 
             thema)
  }else if (subthema == "Podiumkunsten"){
    make_map(dataset, "Afstand tot podiumkunsten (km)",
             niveau, 
             thema)
  }else if (subthema == "Museum"){
    make_map(dataset, "Afstand tot museum (km)",
             niveau, 
             thema)
  }else if (subthema == "Zwembad"){
    make_map(dataset, "Afstand tot zwembad (km)",
             niveau, 
             thema)
  }else if (subthema == "Kunstijsbaan"){
    make_map(dataset, "Afstand tot kunstijsbaan (km)",
             niveau, 
             thema)
  }else if (subthema == "Bibliotheek"){
    make_map(dataset, "Afstand tot bibliotheek (km)",
             niveau, 
             thema)
  }else if (subthema == "Poppodium"){
    make_map(dataset, "Afstand tot poppodium (km)",
             niveau, 
             thema)
  }else if (subthema == "Sauna"){
    make_map(dataset, "Afstand tot sauna (km)",
             niveau, 
             thema)
  }else if (subthema == "Zonnebank"){
    make_map(dataset, "Afstand tot zonnebank (km)",
             niveau, 
             thema)
  }
}

