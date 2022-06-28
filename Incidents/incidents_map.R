# New map with coloring of selected variable
color_map_incidents <- function(df,                     # Dataset of points in selected area and year
                                subthema,               # Subthema selected in input
                                subthema_char,          # Name for subthema in legend title
                                color_incidents,        # Color palette
                                pol_select){            # Selected polygon  
  map <- leaflet(df) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(radius = 5,
                     color = ~color_incidents(subthema),
                     label = ~subthema) %>% 
    addPolylines(data = pol_select,
                 stroke = T,
                 weight = 3) %>%
    addLegend(pal = color_incidents, 
              values = ~subthema,
              title = subthema_char)
  
  return(map)
}