#Dividing the subthemes about health between normal and special (having multiple closely related variables)
Normal <- c("Zeer goede of goede gezondheid (%)", "Langdurige aandoening (%)", 
            "Langdurige ziekte en beperkt (%)",  "(heel) veel stress (%)",
            "Voldoet aan beweegrichtlijn (%)", "Wekelijkse sporters (%)", 
            "Rokers (%)", "Matig tot veel regie over eigen leven (%)", 
            "Mantelzorger (%)", "Vrijwilligerswerk (%)", 
            "Ernstige geluidhinder door buren (%)","Moeite met rondkomen (%)")
Special <- c("Beperking", "Gewicht", "Alcoholgebruik", "Lopen/fietsen naar school of werk", "Eenzaamheid", 
             "Risico op angststoornis of depressie", "Beperkt vanwege gezondheid")

# Function that creates plot for the age distributio of the selected area
age_distribution <- function(data){
  df <- as.data.frame(data)
  df <- df[df$Perioden=="2020" & df$Leeftijd=="18+",]
  df <- df %>% drop_na(Perioden)
  
  selected_area_code <- df[1, "selected_area_code"]
  
  selected_area <- df[df$CODE==selected_area_code,]
  selected_area <- as.data.frame(selected_area)
  selected_area <- select(selected_area,"Personen 0 tot 15 jaar (%)",
                          "Personen 15 tot 25 jaar (%)",
                          "Personen 25 tot 45 jaar (%)",
                          "Personen 45 tot 65 jaar (%)",
                          "Personen 65 jaar en ouder (%)")
  selected_area <- rename(selected_area, c("0 tot 15"=`Personen 0 tot 15 jaar (%)`,
                                           "15 tot 25"= `Personen 15 tot 25 jaar (%)`,
                                           "25 tot 45" = `Personen 25 tot 45 jaar (%)`,
                                           "45 tot 65" = `Personen 45 tot 65 jaar (%)`,
                                           "65+"= `Personen 65 jaar en ouder (%)`))
  selected_area <- rownames_to_column(as.data.frame(t(selected_area)))
  selected_area <- rename(selected_area, c(Leeftijdsgroep = 1, Percentage = 2))
  
  ggplot(selected_area, aes(x = Leeftijdsgroep, y = Percentage)) + geom_col(width = 0.6,position = "dodge",fill = "steelblue3") + 
    coord_flip() + theme(text = element_text(size = 14),axis.text = element_text(size = 12))
}

# Function that create map to point to the selected location and comparable polygons
prime_map_gez <- function(data){
  data <- data[!duplicated(data[ , c("CODE")]), ]
  
  iconblue <- makeAwesomeIcon(
    icon = "arrow-down",
    iconColor = "black",
    markerColor = "blue",
    library = "fa")
  
  leaflet(data) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(color = "navy", weight = 1, 
                highlightOptions = highlightOptions(color = "black", 
                                                    weight = 2),
                label = ~htmlEscape(data$NAAM)) %>% 
    addAwesomeMarkers(lng = data$centroidxx,
                      lat = data$centroidyy,
                      icon = iconblue) 
}

# Function that creates a barplot of the selected area and the mean of comparable areas for all age classes
plot_gez <- function(data, categorie, subtheme){

  if (subtheme %in% Normal){
    column <- subtheme
  }else if (subtheme %in% Special){
    column <- categorie
  }
  
  df <- as.data.frame(data)
  df <- df[df$Perioden=="2020",]
  df <- df %>% drop_na(Perioden)
  
  #Calculating the mean values of the input columns
  df_gem <- select(df, "CODE", "Leeftijd", column)
  df_gem <- df_gem %>% group_by(Leeftijd) %>% 
    summarise_at(vars(column), funs(mean(., na.rm=TRUE)))
  df_gem <- df_gem %>% drop_na(Leeftijd)
  df_gem$groep <- "Gemiddelde"
  
  #Looking for the values of the input columns from the selected areas
  selected_area_code <- df[1, "selected_area_code"]
  selected_area_label <- df[1, "selected_area_label"]
  df_selected <- df %>% subset(CODE == selected_area_code) %>% select("Leeftijd", column)
  df_selected$groep <- selected_area_label
  
  #adding mean and selected together
  df_final <- rbind(df_selected,df_gem)
  df_final$groep <- as.character(df_final$groep)
  df_final$groep <- factor(df_final$groep, levels=unique(df_final$groep))
  df_final$Leeftijd <- as.character(df_final$Leeftijd)
  df_final$Leeftijd <- factor(df_final$Leeftijd, levels=unique(df_final$Leeftijd))
  
  #Plot
  ggplot(df_final, aes(x = Leeftijd, y = .data[[column]], fill = groep)) + geom_col(position = "dodge")+ 
    ylab(column) + theme(text = element_text(size=14),legend.title = element_blank(),legend.position="top",
                         legend.text=element_text(size=12),axis.text = element_text(size = 12)) + 
    scale_x_discrete(labels = function(x)
      stringr::str_wrap(x, width = 15))
}


#Function that creates a line plot for changes over the years
line_plot_gez <- function(data, subtheme, norm_age_line, spec_age_line, categorie_line){
  df <- as.data.frame(data)
  
  #Getting selected age class
  if (subtheme %in% Normal){
    column <- subtheme
    if(norm_age_line == "18-65 65+"){
      df <- df[df$Leeftijd!="18+",]  
    }else{
      df <- df[df$Leeftijd==norm_age_line,]  
    }
  }else if (subtheme %in% Special){
    column <- categorie_line
    if(spec_age_line == "18-65 65+"){
      df <- df[df$Leeftijd!="18+",]  
    }else{
      df <- df[df$Leeftijd==spec_age_line,]  
    }
  }
  df <- df %>% drop_na(Leeftijd)
  
  #Average 
  gem <- select(df,"CODE", "Perioden", "Leeftijd",  column)
  gem <- gem%>%
    group_by(Perioden, Leeftijd) %>%
    summarise_at(vars(column), funs(mean(., na.rm=TRUE)))
  gem <- gem %>% drop_na(Leeftijd)
  gem$groep <- "Gemiddelde"
  
  #Selected area 
  selected_area_code <- df[1, "selected_area_code"]
  selected_area_label <- df[1, "selected_area_label"]
  df_selected <- df %>% subset(CODE == selected_area_code) %>% select("Perioden", "Leeftijd", column)
  df_selected$groep <- selected_area_label
  
  #Merging together
  df_together <- rbind(df_selected, gem)
  df_together$Groep <- as.character(df_together$groep)
  df_together$Groep <- factor(df_together$groep, levels=unique(df_together$Groep))
  
  #Making line plot
  ggplot(df_together, aes(x=Perioden, y=.data[[column]], group=Groep)) +
    geom_line(aes(color=Groep),size=1)+
    geom_point(aes(color=Groep),size=3) +
    labs(x = "Jaar") +
    theme(text = element_text(size=14),legend.title = element_blank(),legend.position="top",
          legend.text=element_text(size=12),axis.text = element_text(size = 12)) + 
    scale_y_continuous(expand = expansion(add = 5)) #To make sure the y-axis has at least 10 percent between the min and max
}

#Function that creates a histogram of the selected subtheme
histogram <- function(data, subtheme, norm_age_hist, categorie_hist, spec_age_hist){
  df <- as.data.frame(data)
  df <- df[df$Perioden=="2020",]
  df <- df %>% drop_na(Perioden)
  
  if (subtheme %in% Normal){
    column <- subtheme
    df <- df[df$Leeftijd==norm_age_hist,]
    df$column <- as.numeric(as.character(df[[subtheme]]))
  }else if (subtheme %in% Special){
    column <- categorie_hist
    df <- df[df$Leeftijd==spec_age_hist,]
    df$column <- as.numeric(as.character(df[[categorie_hist]]))
  }
  
  selected_area_line <- df %>% filter(CODE == selected_area_code) %>%pull(column)
  
  ggplot(df, aes(column)) + geom_histogram(fill= "steelblue3", color='#e9ecef', bins=20) + geom_vline(xintercept = selected_area_line)  +
    labs(x=column, y = "Aantal gebieden") +
    theme(text = element_text(size=14),axis.text = element_text(size = 12))
}

#Function that makes a map of the selected variable containing the comparable areas
make_map_gez <- function(data, age_map, subtheme, categorie_map){
  #get input for the map
  map_data <- data
  map_data <- map_data[map_data$Perioden=="2020" & map_data$Leeftijd==age_map,]
  column <- subtheme

  if (column %in% Normal){
    subtheme <- column
  }else if (column %in% Special){
    subtheme <- categorie_map
  }

  map_data$subtheme <- map_data[[subtheme]]

  #Dividing the subthemes in the higher the better and the lower the better
  higher_better <- c("Zeer goede of goede gezondheid (%)","Voldoet aan beweegrichtlijn (%)","Wekelijkse sporters (%)",
                     "Normaal gewicht (%)","Voldoet aan alcoholrichtlijn (%)","Vrijwilligerswerk (%)",
                     "Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)")
  lower_better <- c("Ondergewicht (%)", "Overgewicht (%)","Ernstig overgewicht (%)","Rokers (%)","Drinkers (%)",
                    "Zware drinkers (%)","Overmatige drinkers (%)","Langdurige aandoening (%)","Beperkt vanwege gezondheid (%)",
                    "Ernstig beperkt vanwege gezondheid (%)","Langdurige ziekte en beperkt (%)","Lichamelijke beperking (%)",
                    "Beperking in horen (%)","Beperking in zien (%)","Beperking in bewegen (%)","Matig tot hoog risico op angststoornis of depressie (%)",
                    "Hoog risico op angststoornis of depressie (%)","(heel) veel stress (%)","Matig tot veel regie over eigen leven (%)",
                    "Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)",
                    "Mantelzorger (%)","Ernstige geluidhinder door buren (%)","Moeite met rondkomen (%)")

  #define colors for polygons and legend (if subtheme in higher the better, reverse=TRUE)
  if(subtheme %in% higher_better){
    pal <- colorBin("YlOrRd", domain = map_data$subtheme, n = 6, reverse = TRUE)
    qpal <- colorQuantile("YlOrRd", map_data$subtheme, n = 6, reverse = TRUE)
  }else if(subtheme %in% lower_better){
    pal <- colorBin("YlOrRd", domain = map_data$subtheme, n = 6)
    qpal <- colorQuantile("YlOrRd", map_data$subtheme, n = 6)
  }

  #for the colors in the map, colorQuantile is used, unless an error is given, then we use colorBin
  coloring <- tryCatch({
    qpal(map_data$subtheme)
  } , error = function(e) {
    pal(map_data$subtheme)
  } )
  legend_title <- as.character(subtheme)
  legend_title <- paste(strwrap(legend_title,20), collapse="<br/>")%>%
    lapply(htmltools::HTML)
  labels <- sprintf("%s: %g", map_data$NAAM, map_data$subtheme) %>%
    lapply(htmltools::HTML)
  #label_content <- sprintf("%s: %g <br/> %s: %g",
  # map_data$selected_area_label, map_data$subtheme[map_data$CODE == map_data$selected_area_code], "Gemiddelde vergelijkbare gebieden", round(mean(map_data$subtheme, na.rm=TRUE), digits = 1))%>% lapply(htmltools::HTML)

  #selected_area_code <- df[1, "selected_area_code"]

  #map
  output_map <- tryCatch({
    leaflet(map_data)%>%
      addPolygons(fillColor = ~coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addMarkers(
        lng = map_data$centroidxx, lat = map_data$centroidyy,
        #label = label_content,
        labelOptions = labelOptions(noHide = T)) %>%
      leaflet::addLegend(pal = qpal, values = ~map_data$subtheme, opacity = 0.7, title = legend_title, position = "bottomright", labFormat = function(type, cuts, p) {      #labformat function makes sure the actual values instead of the quantiles are displayed in the legend
        n = length(cuts)
        paste0(round(cuts[-n],2), " &ndash; ", round(cuts[-1],2))
      }
      )

  }, error = function(e) {
    leaflet(map_data) %>%
      addPolygons(fillColor = ~ coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        lng = map_data$centroidxx, lat = map_data$centroidyy,
        #label = label_content,
        labelOptions = labelOptions(noHide = T))%>%
      leaflet::addLegend(pal = pal, values = ~map_data$subtheme, opacity = 0.7, title = legend_title, position = "bottomright")
  })

  return(output_map)
}


#Function that creates a barplot per categorie if selected subtheme contains multiple variables
staaf_categorie <- function(data, subtheme, spec_age_cat){
  
  #Take the necessary columns, based on what the selected subtheme is
  if(subtheme=="Beperking"){
    columns <- c("Lichamelijke beperking (%)","Beperking in horen (%)", "Beperking in zien (%)", "Beperking in bewegen (%)")
  }else if(subtheme=="Beperkt vanwege gezondheid"){
    columns <- c("Beperkt vanwege gezondheid (%)","Ernstig beperkt vanwege gezondheid (%)")
  }else if(subtheme=="Risico op angststoornis of depressie"){
    columns <- c("Matig tot hoog risico op angststoornis of depressie (%)","Hoog risico op angststoornis of depressie (%)")
  }else if(subtheme=="Gewicht"){
    columns <- c("Ondergewicht (%)","Normaal gewicht (%)","Overgewicht (%)","Ernstig overgewicht (%)")
  }else if(subtheme=="Alcoholgebruik"){
    columns <- c("Voldoet aan alcoholrichtlijn (%)","Drinkers (%)","Zware drinkers (%)","Overmatige drinkers (%)")
  }else if(subtheme=="Lopen/fietsen naar school of werk"){
    columns <- c("Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)")
  }else if(subtheme=="Eenzaamheid"){
    columns <- c("Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)")
  }
  
  df <- as.data.frame(data)
  df <- df[df$Perioden=="2020",]
  df <- df[df$Leeftijd==spec_age_cat,]
  df <- df %>% drop_na(Leeftijd)
  
  #df for average 
  df_gem <- df %>% select(columns)
  df_gem <- as.data.frame(colMeans(x=df_gem, na.rm = TRUE))
  df_gem <- rownames_to_column(df_gem)
  df_gem$groep <- "Gemiddelde"
  df_gem <- rename(df_gem, c(Variabele = 1, Percentage = 2, groep = 3))
  
  #df for selected area
  selected_area_code <- df[1, "selected_area_code"]
  selected_area_label <- df[1, "selected_area_label"]
  df_selected <- df %>% subset(CODE == selected_area_code) %>% select(columns)
  df_selected <- rownames_to_column(as.data.frame(t(df_selected)))
  df_selected$groep <- selected_area_label
  df_selected <- rename(df_selected, c(Variabele = 1, Percentage = 2, groep = 3))
  
  #binding average and selected area together
  df_final <- rbind(df_selected, df_gem)
  
  #Making sure the groeps and variables appear in a consistent order in plot
  df_final$groep <- as.character(df_final$groep)
  df_final$groep <- factor(df_final$groep, levels=unique(df_final$groep))
  df_final$Variabele <- as.character(df_final$Variabele)
  df_final$Variabele <- factor(df_final$Variabele, levels=unique(df_final$Variabele))
  
  ggplot(df_final, aes(x = Variabele, y = Percentage, fill = groep)) + geom_col(position = "dodge") + coord_flip() +
    theme(text = element_text(size=14),legend.title = element_blank(), legend.position="top",
          legend.text=element_text(size=12),axis.text = element_text(size = 12)) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15))
}