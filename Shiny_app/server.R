library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(htmltools)
library(reshape2)

gemeenten <- readRDS("../Data/gemeenten.rds")
wijken <- readRDS("../Data/wijken.rds")
buurten <- readRDS("../Data/buurten.rds")
postcodes_final <- readRDS("../Data/postcodes_final.rds")
full_data <- readRDS("../Data/full_data.rds")
#full_data_crime <- readRDS("../Data/full_data3.rds")
#full_data_crime_norm <- readRDS("../Data/full_data4.rds")

shinyServer(function(input, output, session) {
  
  #remove spaces and change lower case to upper case in postcode
  postcode1 <-  reactive({str_replace_all(input$postcode, fixed(" "), "")})
  postcode <- reactive({toupper(postcode1())})
  
  #Finding gemeente, wijk and buurt based on the input postcode
  output$postcode_info <- renderText(
    if(any(postcodes_final$PC6==postcode())){
      matching_postcode <- postcodes_final %>% filter_at(vars(PC6), any_vars(. %in% postcode()))
      if (nrow(matching_postcode)>1){
        sprintf("Uw postcode komt voor in meerdere gebieden. Uw gemeentenaam is %s, uw wijknaam is %s en uw buurtnaam is %s of uw gemeentenaam is %s, uw wijknaam is %s en uw buurtnaam is %s", 
                matching_postcode$Gemeentenaam2020[1], matching_postcode$wijknaam2020[1], matching_postcode$buurtnaam2020[1], matching_postcode$Gemeentenaam2020[2], matching_postcode$wijknaam2020[2], matching_postcode$buurtnaam2020[2])
        #with(matching_postcode, sprintf('Uw postcode komt voor in meerdere gebieden. Uw gemeentenaam is %s, uw wijknaam is %s en uw buurtnaam is %s', Gemeentenaam2020, wijknaam2020, buurtnaam2020))  
      }else {
        with(matching_postcode, sprintf('Uw gemeentenaam is %s, uw wijknaam is %s en uw buurtnaam is %s', Gemeentenaam2020, wijknaam2020, buurtnaam2020))
      }} else {
        print("Er is (nog) geen geldige postcode ingevoerd.")
      }
  )
  
  # Make selection dependent on previous input
  observeEvent(input$gemeente2, {
    updateSelectInput(session, 'wijken2',
                      choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente2]))  # Only display that are in the selected gemeente
  })
  observeEvent(input$gemeente3, {
    updateSelectInput(session, 'wijken3',
                      choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente3]))  # Only display that are in the selected gemeente
  })
  observeEvent(input$wijken3,{
    updateSelectInput(session, 'buurten3',
                      choices = unique(postcodes_final$buurtnaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente3 & 
                                                                       postcodes_final$wijknaam2020==input$wijken3]))       # Only display buurten that are in the selected wijk
  })
  observeEvent(input$thema, {
    if (input$thema == "Gezondheid en welzijn") {
      updateSelectInput(session, 'subthema', 
                        choices = c("Huisartsenpraktijk", "Ziekenhuis","Apotheek"))
    }else if (input$thema == "Detailhandel") {
      updateSelectInput(session, 'subthema', 
                        choices = c("Supermarkt", "Overige dagelijkse levensmiddelen", "Warenhuis"))
    }else if (input$thema == "Horeca") {
      updateSelectInput(session, 'subthema', 
                        choices = c("Café", "Cafetaria", "Restaurant", "Hotel"))
    }else if (input$thema == "Kinderopvang") {
      updateSelectInput(session, 'subthema', 
                        choices = c("Kinderdagverblijf", "Buitenschoolse opvang"))
    }else if (input$thema == "Onderwijs") {
      updateSelectInput(session, 'subthema', 
                        choices = c("Basisschool", "Voortgezet onderwijs", "VMBO school", "HAVO/VWO school"))
    }else if (input$thema == "Verkeer en vervoer") {
      updateSelectInput(session, 'subthema', 
                        choices = c("Oprit hoofdverkeersweg","Treinstation","Belangrijk overstapstation")) 
    }else if (input$thema == "Vrije tijd en cultuur") {
      updateSelectInput(session, 'subthema', 
                        choices = c("Bioscoop", "Attractie", "Podiumkunsten", "Museum",
                                    "Zwembad", "Kunstijsbaan", "Bibliotheek", "Poppodium","Sauna","Zonnebank"))
    }
  })
  
  #make used data reactive on the selected niveau
  datasetInput <- eventReactive(input$action,{
    df <- as.data.frame(full_data)
    df <- df[df$Niveau == input$niveau,]
    if(input$niveau == 'Gemeenten'){
      if(input$vergelijkbaar1 == "Stedelijkheidsniveau"){
        stedelijkheid_num <- df[df$GM_NAAM == input$gemeente1, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
        comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
      }else if (input$vergelijkbaar1 == "Inkomensniveau"){
        inkomen_num <- df[df$GM_NAAM == input$gemeente1, 'inkomengroep']
        comparable_df <- df[df$inkomengroep == inkomen_num, ]
      } else if(input$vergelijkbaar1 == "Nederland"){
        comparable_df <- df
      }
      comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente1) %>% pull(CODE)
      comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente1) %>% pull(NAAM)
      selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente1)
      row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente1)
    }else if(input$niveau == 'Wijken'){
      if(input$vergelijkbaar2 == "Stedelijkheidsniveau"){
        stedelijkheid_num <- df[df$WK_NAAM == input$wijken2 & df$GM_NAAM == input$gemeente2, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
        comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
        comparable_df <- comparable_df %>% drop_na(CODE)
        output$ink_vergelijkbaarheid <- renderText(
          print("")
        )
        output$opl_vergelijkbaarheid <- renderText(
          print("")
        )
      }else if (input$vergelijkbaar2 == "Inkomensniveau"){
        inkomen_num <- df[df$WK_NAAM == input$wijken2 & df$GM_NAAM == input$gemeente2, 'inkomengroep']
        if(is.na(inkomen_num)){
          comparable_df <- df[df$Niveau == input$niveau,]
          output$ink_vergelijkbaarheid <- renderText(
            print("Let op, door een missende waarde van het inkomensniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
          )
        }else{
          comparable_df <- df[df$inkomengroep == inkomen_num, ]
          comparable_df <- comparable_df %>% drop_na(CODE)
          output$ink_vergelijkbaarheid <- renderText(
            print("")
          )
        }
        
      } else if(input$vergelijkbaar2 == "Nederland"){
        comparable_df <- df
        output$ink_vergelijkbaarheid <- renderText(
          print("")
        )
        output$opl_vergelijkbaarheid <- renderText(
          print("")
        )
      }
      comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2) %>%pull(CODE)
      comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2) %>%pull(NAAM)
      selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2)
      row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente2 & comparable_df$WK_NAAM == input$wijken2)
    }else if(input$niveau == 'Buurten'){
      if(input$vergelijkbaar3 == "Stedelijkheidsniveau"){
        stedelijkheid_num <- df[df$BU_NAAM==input$buurten3 & df$GM_NAAM == input$gemeente3 & df$WK_NAAM == input$wijken3, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
        comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
        comparable_df <- comparable_df %>% drop_na(CODE)
      } else if(input$vergelijkbaar3 == "Nederland"){
        comparable_df <- df
      }
      comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3) %>%pull(CODE)
      comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3) %>%pull(NAAM)
      selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3)
      row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente3 & comparable_df$WK_NAAM == input$wijken3 & comparable_df$BU_NAAM == input$buurten3)
    } 
    comparable_df$centroidxx <- comparable_df[row_num_selected, 'centroidx']
    comparable_df$centroidyy <- comparable_df[row_num_selected, 'centroidy']
    dataset <- st_as_sf(comparable_df)
    
    list_return <- list("dataset" = dataset, "selected_polygon" = selected_polygon)
    return(list_return)
  })
  
  # Get information about selected area in table
  output$info_area <- renderTable({
    df <- as.data.frame(datasetInput()$selected_polygon) %>% 
      select(c("Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)", "inkomengroep", "opleidingsgroep"))
    df <- rename(df, "Stedelijkheid"= `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
    df
  }) 
  
  #Function that returns the 5 most similar areas to the input area based on all voorzieningen variables
  top5_distances_overall <- function(){
    
    df <- as.data.frame(datasetInput()$dataset)
    
    #Making CODE the row index so all rows are identifiable 
    result <-  subset(df, select = -c(CODE))
    row.names(result) <- df$CODE
    
    #subset df so it only contains voorzieningen variables and transform the data with z-score (scale function)
    result <- subset(result, select= `Afstand tot huisartsenpraktijk (km)`: `Aantal musea binnen 20 km`)
    normalized <- as.data.frame(scale(result))
    
    #Making one df for the selected area and one for the comparable areas without the area itself 
    selected_area_code <- df[1, "selected_area_code"]
    selected_area <- normalized[rownames(normalized) == selected_area_code,]
    other <- normalized[rownames(normalized) != selected_area_code,]
    
    #Calculating distance from the selected area to all areas in other dataframe
    dist_matrix <- apply(selected_area,1,function(selected_area)apply(other,1,function(other,selected_area)dist(rbind(other,selected_area),method = 'manhattan'),selected_area))
    dist_df <- as.data.frame(dist_matrix)
    dist_df <- rename(dist_df, "afstand"=1)
    dist_df$CODE <- row.names(dist_df)
    
    #Ordering the distances and returning top 5
    sorted <-  dist_df[order(dist_df$`afstand`),]
    top5 <- head(sorted, 5)
    
    #Merging with original df to get the names of the areas
    final <- merge(top5, df, by="CODE")
    final <-  final[order(final$`afstand`),]      #After merge it was not sorted anymore
    
    return(final)
  }
  
  table_top5_distances_overall <- function(){
    final <- top5_distances_overall()
    
    #If there are no data about the distance to the amenities, return message
    if (is.na(final$afstand)){
      return("Er zijn onvoldoende gegevens beschikbaar voor het geselecteerde gebied")
      
      #Returning GM_NAAM, WK_NAAM and BU_NAAM based on selected niveau
    }else if(input$niveau=="Gemeenten"){
      final <- final %>% select(GM_NAAM)
      final <- rename(final, "Gemeente naam"=GM_NAAM)
      row.names(final) <- NULL
    }else if (input$niveau=="Wijken"){
      final <- final %>% unite(., col = "Wijk naam",  WK_NAAM, GM_NAAM, na.rm=TRUE, sep = " (gemeente ")
      final$`Wijk naam` <- paste0(final$`Wijk naam`, ")")
      final <- final %>% select(`Wijk naam`)
      row.names(final) <- NULL
    }else if (input$niveau=="Buurten"){
      final <- final %>% unite(., col = "Buurt naam",  BU_NAAM, GM_NAAM, na.rm=TRUE, sep = " (gemeente ")
      final <- final %>% unite(., col = "Buurt naam",  `Buurt naam`, WK_NAAM, na.rm=TRUE, sep = ", wijk ")
      final$`Buurt naam` <- paste0(final$`Buurt naam`, ")")
      final <- final %>% select(`Buurt naam`)
      row.names(final) <- NULL
    }
    return(final)
  }
  
  #Gives table output with 5 most similar areas based on all voorzieningen variables
  output$top5_algemeen <- renderTable(
    table_top5_distances_overall(),
    rownames = TRUE
  )
  
  #Function that returns the 5 most similar areas to the input area based on voorzieningen variables in chosen theme
  top5_distances_theme <- function(){
    
    df <- as.data.frame(datasetInput()$dataset)
    
    #Making CODE the row index so all rows are identifiable 
    result <-  subset(df, select = -c(CODE))
    row.names(result) <- df$CODE
    
    #subset df based on theme input so it contains the right voorzieningen variables and transform the data with z-score (scale function)
    if(input$thema=="Gezondheid en welzijn"){
      result <- subset(result, select= `Afstand tot huisartsenpraktijk (km)`: `Aantal ziekenhuizen excl. Buitenpolikliniek binnen 20 km`)
    }else if(input$thema=="Detailhandel"){
      result <- subset(result, select= `Afstand tot grote supermarkt (km)`: `Aantal warenhuizen binnen 20 km`)
    }else if(input$thema=="Horeca"){
      result <- subset(result, select= `Afstand tot cafe (km)`: `Aantal hotel binnen 20 km`)
    }else if(input$thema=="Kinderopvang"){
      result <- subset(result, select= `Afstand tot kinderdagverblijf  (km)`: `Aantal buitenschoolse opvang  binnen 5 km`)
    }else if(input$thema=="Onderwijs"){
      result <- subset(result, select= `Afstand tot basisscholen (km)`: `Aantal scholen HAVO/VWO binnen 10 km`)
    }else if(input$thema=="Verkeer en vervoer"){
      result <- subset(result, select= `Afstand tot oprit hoofdverkeersweg (km)`: `Afstand tot belangrijk overstapstation (km)`)
    }else if (input$thema=="Vrije tijd en cultuur"){
      result <- subset(result, select= `Afstand tot zwembad (km)`: `Aantal musea binnen 20 km`)
    }
    normalized <- as.data.frame(scale(result))
    
    #Making one df for the selected area and one for the comparable areas without the area itself 
    selected_area_code <- df[1, "selected_area_code"]
    selected_area <- normalized[rownames(normalized) == selected_area_code,]
    other <- normalized[rownames(normalized) != selected_area_code,]
    
    #Calculating distance from the selected area to all areas in other dataframe
    dist_matrix <- apply(selected_area,1,function(selected_area)apply(other,1,function(other,selected_area)dist(rbind(other,selected_area),method = 'manhattan'),selected_area))
    dist_df <- as.data.frame(dist_matrix)
    dist_df <- rename(dist_df, "afstand"=1)
    dist_df$CODE <- row.names(dist_df)
    
    #Ordering the distances and returning top 5
    sorted <-  dist_df[order(dist_df$`afstand`),]
    top5 <- head(sorted, 5)
    
    #Merging with original df to get the names of the areas
    final <- merge(top5, df, by="CODE")
    final <-  final[order(final$`afstand`),]      #After merge it was not sorted anymore
    
    return(final)
  }
  
  table_top5_distances_theme <- function(){
    final <- top5_distances_theme()
    #If there are no data about the distance to the amenities, return message
    if (is.na(final$afstand)){
      return("Er zijn onvoldoende gegevens beschikbaar voor het geselecteerde gebied")
      
      #Returning GM_NAAM, WK_NAAM and BU_NAAM based on selected niveau
    } else if(input$niveau=="Gemeenten"){
      final <- final %>% select(GM_NAAM)
      final <- rename(final, "Gemeente naam"=GM_NAAM)
      row.names(final) <- NULL
    }else if (input$niveau=="Wijken"){
      final <- final %>% unite(., col = "Wijk naam",  WK_NAAM, GM_NAAM, na.rm=TRUE, sep = " (gemeente ")
      final$`Wijk naam` <- paste0(final$`Wijk naam`, ")")
      final <- final %>% select(`Wijk naam`)
      row.names(final) <- NULL
    }else if (input$niveau=="Buurten"){
      final <- final %>% unite(., col = "Buurt naam",  BU_NAAM, GM_NAAM, na.rm=TRUE, sep = " (gemeente ")
      final <- final %>% unite(., col = "Buurt naam",  `Buurt naam`, WK_NAAM, na.rm=TRUE, sep = ", wijk ")
      final$`Buurt naam` <- paste0(final$`Buurt naam`, ")")
      final <- final %>% select(`Buurt naam`)
      row.names(final) <- NULL
    }
    return(final)
  }
  
  # Make icon for maps
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
  
  #Function that makes map of the selected variable 
  make_map <- function(variable){
    #get input for the map
    map_data <- datasetInput()$dataset
    map_data$variable <- map_data[[variable]]
    
    #define colors for polygons and legend 
    pal <- colorBin("YlOrRd", domain = map_data$variable)
    qpal <- colorQuantile("YlOrRd", map_data$variable, n = 6)
    #for the colors in the map, colorQuantile is used, unless an error is given, then we use colorBin
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
    
    #map
    output_map <- tryCatch({
      leaflet(map_data)%>%
        addPolygons(fillColor = ~coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                    highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels)%>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        addMarkers(
          lng = map_data$centroidxx, lat = map_data$centroidyy,
          label = label_content,
          labelOptions = labelOptions(noHide = T))%>%
        addAwesomeMarkers(data = top5_distances_theme(),
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
        addAwesomeMarkers(data = top5_distances_theme(),
                          lng = ~centroidx,
                          lat = ~centroidy,
                          icon = icongreen,
                          label = ~NAAM) %>% 
        #addCircleMarkers(lng = map_data$centroidx, lat = map_data$centroidy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
        leaflet::addLegend(pal = pal, values = ~map_data$variable, opacity = 0.7, title = legend_title, position = "bottomright")
    })
    
    return(output_map)
    
  }
  
  #returns table with top 5 similar areas based on chosen theme
  output$top5_theme <- renderTable(
    table_top5_distances_theme(),
    rownames = TRUE
  )
  
  # Create map to point to the selected location and comparable polygons
  output$prime_map <- renderLeaflet({
    leaflet(datasetInput()$dataset) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(color = "navy", weight = 1, 
                  highlightOptions = highlightOptions(color = "black", 
                                                      weight = 2),
                  label = ~htmlEscape(datasetInput()$dataset$NAAM)) %>% 
      addAwesomeMarkers(lng = datasetInput()$dataset$centroidxx,
                        lat = datasetInput()$dataset$centroidyy,
                        icon = iconblue) %>% 
      addAwesomeMarkers(data = top5_distances_overall(),
                        lng = ~centroidx,
                        lat = ~centroidy,
                        icon = iconred,
                        label = ~NAAM)
  })
  
  #Function that takes four column names and creates a barplot of the selected area and the mean of comparable areas
  plot4 <- function(column1, column2, column3){
    
    #Calculating the mean values of the input columns
    df <- as.data.frame(datasetInput()$dataset)
    df_gem <- select(df, column1, column2, column3)
    df_gem <- as.data.frame(colMeans(df_gem, na.rm = TRUE))
    df_gem <-  rownames_to_column(df_gem)
    df_gem$groep <- "Gemiddelde"
    df_gem <- rename(df_gem, c(Variabele = 1, Aantal = 2, groep=3))
    
    #Looking for the values of the input columns from the selected areas
    selected_area_code <- df[1, "selected_area_code"]
    selected_area_label <- df[1, "selected_area_label"]
    df_selected <- df %>% subset(CODE == selected_area_code) %>% select(column1, column2, column3)
    df_selected <- rownames_to_column(as.data.frame(t(df_selected)))
    df_selected$groep <- selected_area_label
    df_selected <- rename(df_selected, c(Variabele = 1, Aantal = 2, groep = 3))
    
    #adding mean and selected together
    df_final <- rbind(df_selected, df_gem)
    df_final$groep <- as.character(df_final$groep)
    df_final$groep <- factor(df_final$groep, levels=unique(df_final$groep))
    df_final$Variabele <- as.character(df_final$Variabele)
    df_final$Variabele <- factor(df_final$Variabele, levels=unique(df_final$Variabele))
    
    #Plot
    ggplot(df_final, aes(x = Variabele, y = Aantal, fill = groep)) + geom_col(position = "dodge") + 
      theme(text = element_text(size = 14),legend.title = element_blank())+
      scale_x_discrete(labels = function(x) 
        stringr::str_wrap(x, width = 15))
  }
  
  
  #Maps for all variables with distance to closest spot
  output$map_variable <- renderLeaflet({
    if (input$subthema == "Huisartsenpraktijk"){
      make_map("Afstand tot huisartsenpraktijk (km)")
    }else if (input$subthema == "Ziekenhuis"){
      make_map("Afstand tot ziekenhuis incl. buitenpolikliniek (km)")
    }else if(input$subthema=="Apotheek"){
      make_map("Afstand tot apotheek (km)")
    }else if (input$subthema == "Supermarkt"){
      make_map("Afstand tot grote supermarkt (km)")
    }else if (input$subthema == "Overige dagelijkse levensmiddelen"){
      make_map("Afstand tot overige dagelijkse levensmiddelen (km)")
    }else if (input$subthema == "Warenhuis"){
      make_map("Afstand tot warenhuis (km)")
    }else if (input$subthema == "Café"){
      make_map("Afstand tot cafe (km)")
    }else if (input$subthema == "Cafetaria"){
      make_map("Afstand tot cafetaria (km)")
    }else if (input$subthema == "Restaurant"){
      make_map("Afstand tot restaurant (km)")
    }else if (input$subthema == "Hotel"){
      make_map("Afstand tot hotel (km)")
    }else if (input$subthema == "Kinderdagverblijf"){
      make_map("Afstand tot kinderdagverblijf  (km)")
    }else if (input$subthema == "Buitenschoolse opvang"){
      make_map("Afstand tot buitenschoolse opvang  (km)")
    }else if (input$subthema == "Basisschool"){
      make_map("Afstand tot basisscholen (km)")
    }else if (input$subthema == "Voortgezet onderwijs"){
      make_map("Afstand tot voortgezet onderwijs (km)")
    }else if (input$subthema == "VMBO school"){
      make_map("Afstand tot scholen VMBO (km)")
    }else if (input$subthema == "HAVO/VWO school"){
      make_map("Afstand tot scholen HAVO/VWO (km)")
    }else if (input$subthema == "Oprit hoofdverkeersweg"){
      make_map("Afstand tot oprit hoofdverkeersweg (km)")
    }else if (input$subthema == "Treinstation"){
      make_map("Afstand tot treinstation (km)")
    }else if (input$subthema == "Belangrijk overstapstation"){
      make_map("Afstand tot belangrijk overstapstation (km)")
    }else if (input$subthema == "Bioscoop"){
      make_map("Afstand tot bioscoop (km)")
    }else if (input$subthema == "Attractie"){
      make_map("Afstand tot attractie (km)")
    }else if (input$subthema == "Podiumkunsten"){
      make_map("Afstand tot podiumkunsten (km)")
    }else if (input$subthema == "Museum"){
      make_map("Afstand tot museum (km)")
    }else if (input$subthema == "Zwembad"){
      make_map("Afstand tot zwembad (km)")
    }else if (input$subthema == "Kunstijsbaan"){
      make_map("Afstand tot kunstijsbaan (km)")
    }else if (input$subthema == "Bibliotheek"){
      make_map("Afstand tot bibliotheek (km)")
    }else if (input$subthema == "Poppodium"){
      make_map("Afstand tot poppodium (km)")
    }else if (input$subthema == "Sauna"){
      make_map("Afstand tot sauna (km)")
    }else if (input$subthema == "Zonnebank"){
      make_map("Afstand tot zonnebank (km)")
    }
  })
  
  #Maps for the amount of instances inside a radius
  output$plot_variable <- renderPlot({
    if (input$subthema == "Huisartsenpraktijk"){
      plot4("Aantal huisartsenpraktijken binnen 1 km", 
            "Aantal huisartsenpraktijken binnen 3 km", 
            "Aantal huisartsenpraktijken binnen 5 km")
    }else if (input$subthema == "Ziekenhuis"){
      plot4("Aantal ziekenhuizen incl. buitenpolikliniek binnen 5 km",                        
            "Aantal ziekenhuizen incl. buitenpolikliniek binnen 10 km",                       
            "Aantal ziekenhuizen incl. buitenpolikliniek binnen 20 km")
    }else if (input$subthema == "Supermarkt"){
      plot4("Aantal  grote supermarkten binnen 1 km",                                         
            "Aantal  grote supermarkten binnen 3 km",                                          
            "Aantal  grote supermarkten binnen 5 km")
    }else if (input$subthema == "Overige dagelijkse levensmiddelen"){
      plot4("Aantal winkels overige dagelijkse levensmiddelen binnen 1 km",                    
            "Aantal winkels overige dagelijkse levensmiddelen binnen 3 km",                    
            "Aantal winkels overige dagelijkse levensmiddelen binnen 5 km")
    }else if (input$subthema == "Warenhuis"){
      plot4("Aantal warenhuizen binnen 5 km",                                                  
            "Aantal warenhuizen binnen 10 km",                                                
            "Aantal warenhuizen binnen 20 km")
    }else if (input$subthema == "Café"){
      plot4("Aantal cafes binnen 1 km" ,                                                       
            "Aantal cafes binnen 3 km" ,                                                       
            "Aantal cafes binnen 5 km")
    }else if (input$subthema == "Cafetaria"){
      plot4("Aantal cafetaria's binnen 1 km",                                                  
            "Aantal cafetaria's binnen 3 km",                                                  
            "Aantal cafetaria's binnen 5 km")
    }else if (input$subthema == "Restaurant"){
      plot4("Aantal restaurants binnen 1 km",                                                  
            "Aantal restaurants binnen 3 km",                                                  
            "Aantal restaurants binnen 5 km")
    }else if (input$subthema == "Hotel"){
      plot4("Aantal hotel binnen 5 km",                                                  
            "Aantal hotel binnen 10 km",                                                  
            "Aantal hotel binnen 20 km")
    }else if (input$subthema == "Kinderdagverblijf"){
      plot4("Aantal kinderdagverblijf  binnen 1 km",                                                  
            "Aantal kinderdagverblijf  binnen 3 km",                                                  
            "Aantal kinderdagverblijf  binnen 5 km")
    }else if (input$subthema == "Buitenschoolse opvang"){
      plot4("Aantal buitenschoolse opvang  binnen 1 km",                                                  
            "Aantal buitenschoolse opvang  binnen 3 km",                                                  
            "Aantal buitenschoolse opvang  binnen 5 km")
    }else if (input$subthema == "Basisschool"){
      plot4("Aantal basisscholen binnen 1 km",                                                  
            "Aantal basisscholen binnen 3 km",                                                  
            "Aantal basisscholen binnen 5 km")
    }else if (input$subthema == "Voortgezet onderwijs"){
      plot4("Aantal voortgezet onderwijs binnen 3 km",                                                  
            "Aantal voortgezet onderwijs binnen 5 km",                                                  
            "Aantal voortgezet onderwijs binnen 10 km")
    }else if (input$subthema == "VMBO school"){
      plot4("Aantal scholen VMBO binnen 3 km",                                                  
            "Aantal scholen VMBO binnen 5 km",                                                  
            "Aantal scholen VMBO binnen 10 km")
    }else if (input$subthema == "HAVO/VWO school"){
      plot4("Aantal scholen HAVO/VWO binnen 3 km",                                                  
            "Aantal scholen HAVO/VWO binnen 5 km",                                                  
            "Aantal scholen HAVO/VWO binnen 10 km")
    }else if (input$subthema == "Bioscoop"){
      plot4("Aantal bioscoop binnen 5 km",                                                  
            "Aantal bioscoop binnen 10 km",                                                  
            "Aantal bioscoop binnen 20 km")
    }else if (input$subthema == "Attractie"){
      plot4("Aantal attracties binnen 10 km",                                                  
            "Aantal attracties binnen 20 km",                                                  
            "Aantal attracties binnen 50 km")
    }else if (input$subthema == "Podiumkunsten"){
      plot4("Aantal podiumkunsten binnen 5 km",                                                  
            "Aantal podiumkunsten binnen 10 km",                                                  
            "Aantal podiumkunsten binnen 20 km")
    }else if (input$subthema == "Museum"){
      plot4("Aantal musea binnen 5 km",                                                  
            "Aantal musea binnen 10 km",                                                  
            "Aantal musea binnen 20 km")
    }
  })
  
  #Selected area name for the box titles (changes only when 'zoeken' button is clicked)
  selected_area_title <- eventReactive(input$action,{
    if(input$niveau=="Gemeenten"){
      title <- input$gemeente1
    }else if(input$niveau=="Wijken"){
      title <- input$wijken2
    }else if(input$niveau=="Buurten"){
      title <- input$buurten3
    }
    return(title)
  })
  
  #Info box, title changes based on the selected area
  output$info_box = renderUI({
    title <- paste0("Informatie over: ", selected_area_title())
    box(title = title, width = 3, status = "warning", solidHeader = T,
        "In de tabel hieronder ziet u wat de stedelijkheid, de inkomensgroep en de opleidingsgroep zijn voor het gekozen gebied.",
        tableOutput("info_area"),
        "Stedelijkheid: 1 = zeer sterk stedelijk, 5 = niet stedelijk.", br(),
        "Inkomensniveau: 1 = zeer laag percentage, 4 = hoog percentage van huishoudens met een inkomen onder het sociaal minimum.",br(),
        "Opleidingsniveau: 1 = zeer laag percentage, 4 = zeer hoog percentage van personen met een lage opleiding.",
        span(textOutput("ink_vergelijkbaarheid"), style="color:red"),
        span(textOutput("opl_vergelijkbaarheid"), style="color:red")) # Box informatie 
  })
  
  #Kaart box, title changes based on the selected area
  output$kaart_box = renderUI({
    title <- title <- paste0("Kaart met: ", selected_area_title())
    box(title = title, width = 4, status = "warning", solidHeader = T,
        "Kaart waarop het gekozen gebied te zien is (blauwe pointer), de top 5 meest vergelijkbare gebieden (rode pointers) en de gebieden waarmee wordt vergeleken.",
        #"Hier komt de prime map van leaflet met pointer naar centroid van de geselecteerde g/w/b",
        shinycssloaders::withSpinner(leafletOutput("prime_map"))) 
  })
  
  #Top 5 voor thema, title changes based on the selected theme
  output$top5 = renderUI({
    title <- paste0("Top 5 ",input$thema)
    box(title = title, width = NULL, background = "green",
        "Top 5 met vergelijkbare gebieden voor het gekozen thema",
        #"Hier komt de top 5 van vergelijkbare g/w/b voor een bepaald thema",
        tableOutput('top5_theme')) 
  })
  
  #Top 5 voor thema, title changes based on the selected theme
  output$kaartNL = renderUI({
    title <- paste0("Kaart van Nederland: ",input$subthema)
    box(title = title, width = 6, status = "warning", solidHeader = T,
        "Kaart van Nederland met het gekozen subthema.",
        #"Hier komt de kaart van Nederland met geselecteerde vergelijkbare g/w/b op bepaalde variabele",
        shinycssloaders::withSpinner(leafletOutput("map_variable"))) 
  })
  
  #Creates box for staafdiagram only when one of the subthemes is selected that has one
  output[["box_staafdiagram"]] <- renderUI({
    
    #subthemes that have data available on the count inside a radius
    subthemes_count <- c("Huisartsenpraktijk","Ziekenhuis", "Supermarkt", "Overige dagelijkse levensmiddelen", "Warenhuis",
                         "Café", "Cafetaria", "Restaurant", "Hotel", "Kinderdagverblijf", "Buitenschoolse opvang", "Basisschool",
                         "Voortgezet onderwijs", "VMBO school", "HAVO/VWO school", "Bioscoop", "Attractie", "Podiumkunsten", "Museum")
    
    title <- paste0("Staafdiagram: ",input$subthema)
    
    #if the selected subthemes is in these subthemes with count, show the "staafdiagram" box
    if(input$subthema %in% subthemes_count){
      box(title = title, width = 4, status = "warning", solidHeader = T,
          "Aantal van het gekozen subthema binnen een bepaalde afstand, voor het gekozen gebied (roze) en andere vergelijkbare gebieden (blauw).",
          #"Hier komt een staafdiagram om je wijk te vergelijken met het gemiddelde van vergelijkbare wijken",
          plotOutput("plot_variable"))
    }
  })
  
  ## CRIME ##
  
  # Make selection dependent on previous input
  observeEvent(input$gemeente_crime2, {
    updateSelectInput(session, 'wijken_crime2',
                      choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente_crime2]))  # Only display that are in the selected gemeente
  })
  observeEvent(input$gemeente_crime3, {
    updateSelectInput(session, 'wijken_crime3',
                      choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente_crime3]))  # Only display that are in the selected gemeente
  })
  observeEvent(input$wijken_crime3,{
    updateSelectInput(session, 'buurten_crime3',
                      choices = unique(postcodes_final$buurtnaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente_crime3 & 
                                                                       postcodes_final$wijknaam2020==input$wijken_crime3]))       # Only display buurten that are in the selected wijk
  }) 
  
  #select right input based on input at thema_crime
  observeEvent(input$thema_crime, {
    if (input$thema_crime == "Totaal misdrijven") {
      updateSelectInput(session, 'soort_crime', 
                        choices = c("Totaal misdrijven"))
    }else if (input$thema_crime == "Vermogensdelicten") {
      updateSelectInput(session, 'soort_crime', 
                        choices = c("Diefstal/inbraak woning", "Diefstal/inbraak box/garage/schuur", "Diefstal uit/vanaf motorvoertuigen","Diefstal van motorvoertuigen","Diefstal van brom-, snor-, fietsen",
                                    "Zakkenrollerij", "Diefstal af/uit/van overige voertuigen", "Horizontale fraude", "Verticale fraude", "Fraude (overig)","Diefstal/inbraak bedrijven", "Winkeldiefstal", 
                                    "Diefstallen (water)", "Overige vermogensdelicten"))
    }else if (input$thema_crime == "Gewelds- en seksuele misdrijven") {
      updateSelectInput(session, 'soort_crime', 
                        choices = c("Zedenmisdrijf","Moord, doodslag", "Openlijk geweld (persoon)", "Bedreiging", "Mishandeling", "Straatroof", "Overval", "Kinderporno", "Kinderprostitutie"))
    }else if (input$thema_crime == "Vernielingen en misdrijven tegen openbare orde en gezag") {
      updateSelectInput(session, 'soort_crime', 
                        choices = c("Aantasting openbare orde", "Discriminatie", "Brand/ontploffing","Vernieling cq. zaakbeschadiging","Huisvredebreuk", "Leefbaarheid (overig)", "Vreemdelingenzorg", 
                                    "Maatschappelijke intergriteit (overig)", "Cybercrime", "Burengerucht (relatieproblemen)"))
    }else if (input$thema_crime == "Verkeersmisdrijven") {
      updateSelectInput(session, 'soort_crime', 
                        choices = c("Onder invloed (lucht)", "Lucht (overig)", "Onder invloed (water)", "Onder invloed (weg)", "Weg (overig)", "Ongevallen (weg)"))
    }else if (input$thema_crime == "Misdrijven omgeving en milieu") {
      updateSelectInput(session, 'soort_crime', 
                        choices = c("Inrichting Wet Milieubeheer", "Bodem","Water", "Afval", "Bouwstoffen", "Mest", "Transport gevaarlijke stoffen", "Vuurwerk", "Bestrijdingsmiddelen", 
                                    "Natuur en landschap", "Ruimtelijke ordening", "Dieren", "Voedselveiligheid")) 
    }else if (input$thema_crime == "Overige misdrijven") {
      updateSelectInput(session, 'soort_crime', 
                        choices = c("Mensenhandel","Drugs/drankoverlast", "Drugshandel","Mensensmokkel", "Wapenhandel", "Bijzondere wetten" ))
    }
  })
  
  #select right dataset based on aantallen 
  crime_data <- eventReactive(input$aantal_crime,{
    if (input$aantal_crime == "Aantal misdrijven"){
      crime_data <- full_data_crime
    } else if (input$aantal_crime == "Aantallen per 1000 inwoners"){
      crime_data <- full_data_crime_norm
    } 
  }
  )
  
  ###
  #vergelijkbaarheid
  ###
  
  #make used data reactive on the selected niveau
  datasetInputCrime <- eventReactive(input$action_crime,{
    df <- as.data.frame(crime_data())
    df <- df[df$Niveau == input$niveau_crime,]
    df <- df %>% drop_na("CODE")
    if(input$niveau_crime == 'Gemeenten'){
      if(input$vergelijkbaar_crime1 == "Stedelijkheidsniveau"){
        stedelijkheid_num <- df[df$GM_NAAM == input$gemeente_crime1, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
        comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
      }else if (input$vergelijkbaar_crime1 == "Inkomensniveau"){
        inkomen_num <- df[df$GM_NAAM == input$gemeente_crime1, 'inkomengroep']
        comparable_df <- df[df$inkomengroep == inkomen_num, ]
      }else if (input$vergelijkbaar_crime1 == "Opleidingsniveau"){
        opleiding_num <- df[df$GM_NAAM == input$gemeente_crime1, 'opleidingsgroep']
        comparable_df <- df[df$opleidingsgroep == opleiding_num, ]
      } else if(input$vergelijkbaar_crime1 == "Nederland"){
        comparable_df <- df
      }
      comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente_crime1) %>% pull(CODE)
      comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente_crime1) %>% pull(NAAM)
      selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente_crime1)
      row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente_crime1)
    }else if(input$niveau_crime == 'Wijken'){
      if(input$vergelijkbaar_crime2 == "Stedelijkheidsniveau"){
        stedelijkheid_num <- df[df$WK_NAAM == input$wijken_crime2 & df$GM_NAAM == input$gemeente_crime2, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
        comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
        comparable_df <- comparable_df %>% drop_na(CODE)
        output$ink_vergelijkbaarheid <- renderText(
          print("")
        )
        output$opl_vergelijkbaarheid <- renderText(
          print("")
        )
      }else if (input$vergelijkbaar_crime2 == "Inkomensniveau"){
        inkomen_num <- df[df$WK_NAAM == input$wijken_crime2 & df$GM_NAAM == input$gemeente_crime2, 'inkomengroep']
        if(is.na(inkomen_num)){
          comparable_df <- df[df$Niveau == input$niveau_crime,]
          output$ink_vergelijkbaarheid <- renderText(
            print("Let op, door een missende waarde van het inkomensniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
          )
        }else{
          comparable_df <- df[df$inkomengroep == inkomen_num, ]
          comparable_df <- comparable_df %>% drop_na(CODE)
          output$ink_vergelijkbaarheid <- renderText(
            print("")
          )
        }
      }else if (input$vergelijkbaar_crime2 == "Opleidingsniveau"){
        opleiding_num <- df[df$WK_NAAM == input$wijken_crime2 & df$GM_NAAM == input$gemeente_crime2, 'opleidingsgroep']
        if(is.na(opleiding_num)){
          comparable_df <- df[df$Niveau == input$niveau_crime,]
          output$opl_vergelijkbaarheid <- renderText(
            print("Let op, door een missende waarde van het opleidingsniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
          )
        }else{
          comparable_df <- df[df$opleidingsgroep == opleiding_num, ]
          comparable_df <- comparable_df %>% drop_na(CODE)
          output$opl_vergelijkbaarheid <- renderText(
            print("")
          )
        }
      } else if(input$vergelijkbaar_crime2 == "Nederland"){
        comparable_df <- df
        output$ink_vergelijkbaarheid <- renderText(
          print("")
        )
        output$opl_vergelijkbaarheid <- renderText(
          print("")
        )
      }
      comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente_crime2 & WK_NAAM == input$wijken_crime2) %>%pull(CODE)
      comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente_crime2 & WK_NAAM == input$wijken_crime2) %>%pull(NAAM)
      selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente_crime2 & WK_NAAM == input$wijken_crime2)
      row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente_crime2 & comparable_df$WK_NAAM == input$wijken_crime2)
    }else if(input$niveau_crime == 'Buurten'){
      if(input$vergelijkbaar_crime3 == "Stedelijkheidsniveau"){
        stedelijkheid_num <- df[df$BU_NAAM==input$buurten_crime3 & df$GM_NAAM == input$gemeente_crime3 & df$WK_NAAM == input$wijken_crime3, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
        comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
        comparable_df <- comparable_df %>% drop_na(CODE)
      } else if(input$vergelijkbaar_crime3 == "Nederland"){
        comparable_df <- df
      }
      comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente_crime3 & WK_NAAM == input$wijken_crime3 & BU_NAAM == input$buurten_crime3) %>%pull(CODE)
      comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente_crime3 & WK_NAAM == input$wijken_crime3 & BU_NAAM == input$buurten_crime3) %>%pull(NAAM)
      selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente_crime3 & WK_NAAM == input$wijken_crime3 & BU_NAAM == input$buurten_crime3)
      row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente_crime3 & comparable_df$WK_NAAM == input$wijken_crime3 & comparable_df$BU_NAAM == input$buurten_crime3)
    } 
    comparable_df$centroidxx <- comparable_df[row_num_selected, 'centroidx']
    comparable_df$centroidyy <- comparable_df[row_num_selected, 'centroidy']
    dataset <- st_as_sf(comparable_df)
    
    list_return <- list("dataset" = dataset, "selected_polygon" = selected_polygon)
    return(list_return)
  })
  
  
  ########
  #Plot
  ########
  
  output$crime_plot <- renderPlot({
    make_crime_plot(input$soort_crime)
  })
  
  make_crime_plot <- function(type_of_crime){
    
    #plot output for selected area 
    df <- as.data.frame(datasetInputCrime()$selected_polygon)
    selected_area_label <- df[1, "selected_area_label"]
    df_selected <- select(df, ends_with(type_of_crime))
    df_selected[df_selected == Inf] <- 0 #if number is too small assign 0
    df_selected <- rownames_to_column(df_selected)
    #df_selected <- na.omit(df_selected)
    df_selected <- df_selected[,-1]
    colnames(df_selected) <- gsub("([A-Za-z]+).*", "", colnames(df_selected))
    df_selected$group <- selected_area_label
    
    #plot output mean for the selected vergelijkbare gebieden
    df_comp <- as.data.frame(datasetInputCrime()$dataset)
    df_comp <- select(df_comp, ends_with(type_of_crime))
    df_comp[df_comp == Inf] <- 0    #if number is too small assign 0 
    df_means <- as.data.frame(round(colMeans(df_comp, na.rm = TRUE)))
    df_means <- rownames_to_column(df_means)
    df_means <- melt(df_means)
    df_means <- dcast(df_means, variable ~ rowname, var.value=value)
    df_means <- df_means[,-1]
    colnames(df_means) <- gsub("([A-Za-z]+).*", "", colnames(df_means))
    df_means$group <- "Gemiddelde van de vergeleken gebieden"
    
    #combine plot output 
    df_total <- rbind(df_selected, df_means)
    df_total$group <- as.character(df_total$group)
    df_total$group <- factor(df_total$group, levels=unique(df_total$group))
    df_total <- melt(df_total)
    
    #define plot details 
    ggplot(data = df_total, aes(x= variable, y=value, color = group, group = group)) + ggtitle(type_of_crime) + geom_point(size = 3)+ geom_line(size = 0.75)+
      labs(x = "Jaartal", y = "Aantal") + theme_minimal() +  
      theme(text = element_text(size = 14),legend.title = element_blank()) 
  }
  
  
  #Function that makes map of the selected variable 
  make_crime_map <- function(year, variable){
    
    #get input for the map
    map_data <- datasetInputCrime()$dataset
    selected_map_data <- select(map_data, ends_with(variable))          #select crime
    selected_map_data <- select(selected_map_data, starts_with(year))   #select year
    #selected_map_data[selected_map_data == Inf] <- 0                    #if number is too small assign 0 
    colnames(selected_map_data) <- "selected_variable"
    map_data$variable <- selected_map_data[["selected_variable"]] 
    
    #define colors for polygons and legend 
    pal <- colorBin("YlOrRd", domain = map_data$variable)
    qpal <- colorQuantile("YlOrRd", map_data$variable, n = 6)
    #for the colors in the map, colorQuantile is used, unless an error is given, then we use colorBin
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
    
    #map
    output_map <- tryCatch({
      leaflet(map_data)%>%
        addPolygons(fillColor = ~coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                    highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels)%>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        addMarkers(
          lng = map_data$centroidxx, lat = map_data$centroidyy,
          label = label_content,
          labelOptions = labelOptions(noHide = T))%>%
        #addCircleMarkers(lng = map_data$centroidxx, lat = map_data$centroidyy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
        leaflet::addLegend(pal = qpal, values = ~map_data$variable, opacity = 0.7, title = legend_title, position = "bottomright", labFormat = function(type, cuts, p) {      #labformat function makes sure the actual values instead of the quantiles are displayed in the legend
          n = length(cuts)
          paste0(round(cuts[-n],0), " &ndash; ", round(cuts[-1],0))
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
        
        #addCircleMarkers(lng = map_data$centroidx, lat = map_data$centroidy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
        leaflet::addLegend(pal = pal, values = ~map_data$variable, opacity = 0.7, title = legend_title, position = "bottomright")
    })
    
    return(output_map)
    
  }
  output$crime_map <- renderLeaflet({
    make_crime_map(input$jaar_crime, input$soort_crime)
  })
  
  ### TOP5 ####
  
  top5_crime <- function(type_of_crime, year){
    df <- as.data.frame(datasetInputCrime()$dataset)
    df_selected_area <- as.data.frame(datasetInputCrime()$selected_polygon)
    
    if (input$niveau_crime == "Gemeenten"){
      df_var <- select(df,CODE, NAAM, ends_with(type_of_crime))     #select crime
      df_var_year <- select(df_var, CODE, NAAM, starts_with(year))  #select year
      df_var_year[df_var_year == Inf] <- 0                          #if number is too small assign 0 
      names(df_var_year) <- c("CODE", "Gemeentenaam", "Aantal")
    }else if (input$niveau_crime == "Wijken"){
      df_var <- select(df, CODE, NAAM, GM_NAAM, ends_with(type_of_crime))
      df_var_year <- select(df_var, CODE, NAAM, GM_NAAM, starts_with(year))
      df_var_year[df_var_year == Inf] <- 0    
      names(df_var_year) <- c("CODE", "Wijk", "Gemeentenaam", "Aantal")
    }else if (input$niveau_crime == "Buurten"){
      df_var <- select(df, CODE, NAAM, GM_NAAM, ends_with(type_of_crime))
      df_var_year <- select(df_var, CODE, NAAM, GM_NAAM, starts_with(year))
      df_var_year[df_var_year == Inf] <- 0   
      names(df_var_year) <- c("CODE", "Buurt", "Gemeentenaam", "Aantal")
    }
    
    x <- ncol(df_var_year)                                      #get number of last column 
    df_top5 <- arrange(df_var_year, desc(df_var_year[ ,x]))     #arrange by the number of the last column
    df_top5$Rank <- 1:nrow(df_top5)                             #give ranknumber
    df_top5_selected_area <- df_top5[df_top5$CODE == df_selected_area$CODE,]  #select the selected area
    df_top5_5 <- df_top5 %>% slice(1:5)                         #select the top5
    df_top5 <- rbind(df_top5_5, df_top5_selected_area)          #bind together top5 and selected area
    df_top5 <- df_top5 %>% distinct(CODE, .keep_all = TRUE)
    df_top5$CODE <- NULL
    df_top5 <- df_top5 %>% select("Rank", everything())         #make rank first column
    df_top5$Aantal <- round(df_top5$Aantal, digits = 0)
    return(df_top5)
  }
  
  output$top5_crime2 <- renderTable(
    top5_crime(input$soort_crime, input$jaar_crime)
  )
  
  output$top5_crime = renderUI({
    title <- paste0("Top 5 ",input$soort_crime, " ", input$jaar_crime)
    box(title = title, width = NULL, background = "red",
        "Top 5 waar misdrijf het meest voorkomt",
        #"Hier komt de top 5 van vergelijkbare g/w/b voor een bepaald thema",
        tableOutput("top5_crime2")) 
  })
  
  #information on the type of crime
  output$info_crime <- renderText({
    if (input$soort_crime == "Totaal misdrijven"){
      "Totaal van alle misdrijven"
    } else if (input$soort_crime == "Diefstal/inbraak woning"){
      "Diefstal d.m.v. of zonder braak, verbreking, inklimming, valse sleutel, valse order of vals kostuum in/uit een ruimte waar men
          woont alsmede de schuren, boxen, garages, bergingen, etc. die men rechtstreeks vanuit de woning kan betreden. Eventueel vergezeld of
          gevolgd door (bedreiging met) geweld gericht tegen personen in/uit een ruimte waar men woont alsmede de
          schuren, boxen, garages, bergingen, etc. die men rechtstreeks vanuit de woning kan betreden."
    } else if (input$soort_crime == "Diefstal/inbraak box/garage/schuur"){
      "Diefstal d.m.v.of zonder braak, verbreking, inklimming, valse sleutel, valse order of vals kostuum uit particuliere
          schuren/boxen/garages/tuinhuisjes die niet rechtstreeks verbonden zijn met een woning. Eventueel vergezeld of
            gevolgd door (bedreiging met) geweld gericht tegen personen uit particuliere schuren/boxen/garages/tuinhuisjes
              die niet rechtstreeks verbonden zijn met een woning."
    } else if (input$soort_crime == "Diefstal uit/vanaf motorvoertuigen"){
      "De diefstal (ook d.m.v. braak, verbreking, inklimming, valse sleutel, valse order of vals kostuum) van voorwerpen of
            onderdelen, gepleegd vanaf of uit een personenauto.Eventueel vergezeld of gevolgd door (bedreiging met) geweld gericht tegen personen uit/vanaf een personenauto op de
              openbare weg of een voor het publiek vrij toegankelijke plaats. De personenauto moet zich bevinden op een locatie waarvoor
              geen specifieke andere maatschappelijke klasse beschikbaar is. De personenauto dient zich te bevinden op of aan
                de openbare weg of openbaar toegankelijke cq niet fysiek afgeschermde plaats. "
    }else if (input$soort_crime == "Diefstal van motorvoertuigen"){
      "Diefstal van een personenauto/motorfiets/auto voor vrachtvervoer op de openbare weg of een voor het publiek vrij toegankelijke plaats. Eventueel vergezeld of gevolgd door geweld (gericht tegen personen) van een personenauto/motorfiets/auto voor vrachtvervoer die
          gepleegd is op of aan de openbare weg of openbaar toegankelijke plaats"
    }else if (input$soort_crime == "Diefstal van brom-, snor-, fietsen"){
      "Diefstal van een fiets/bromfiets/snorfiets op de openbare weg of een voor het publiek vrij toegankelijke plaats. Eventueel vergezeld of gevolgd door geweld (gericht tegen personen) van een fiets/bromfiets/snorfiets die gepleegd is op
          of aan de openbare weg of openbaar toegankelijke plaats."
    }else if (input$soort_crime == "Zakkenrollerij"){
      "Heimenlijk (dus zonder geweld of bedreiging met geweld) wegnemen van geld en/of andere goederen op of aan het
        lichaam gedragen of uit kleding die door het slachtoffer gedragen wordt. Voor tassenrollerij dient het weggenomen
        goed in een (rug-/plastic-/of andere) tas gezeten te hebben. Die tas moet gedragen zijn door de betrokkene."
    }else if (input$soort_crime == "Diefstal af/uit/van overige voertuigen"){
      "De diefstal (ook d.m.v. braak, verbreking, inklimming, valse sleutel, valse order of vals kostuum) van voorwerpen of
          onderdelen vanaf of uit een vervoermiddel met wielen of glijvlakken voor het vervoer over land van personen en/of goederen (niet zijnde een personenauto,
          motor, fiets, bromfiets/snorfiets, vrachtauto en bestelauto). Dit vervoermiddel dient zich
          te bevinden op of aan de openbare weg of openbaar toegankelijke cq niet fysiek afgeschermde plaats.Eventueel , vergezeld of gevolgd door geweld (gericht tegen personen) van een vervoermiddel met
          wielen of glijvlakken voor het vervoer over land van personen en/of goederen (niet zijnde een personenauto,
          motor, fiets, bromfiets/snorfiets, vrachtauto en bestelauto), die gepleegd is op of aan de openbare weg of openbaar
          toegankelijke plaats. "
    }else if (input$soort_crime == "Ongevallen (weg)"){
      "Een gebeurtenis die verband houdt met het verkeer op een openbare weg voor het rijverkeer en ander verkeer, ten gevolge waarvan bij een of meerdere weggebruikers letsel bij personen ontstaat waarbij een medische behandeling noodzakelijk is of ten gevolge waarvan een of meerdere weggebruikers zijn overleden en (schade ontstaat aan objecten) waarbij minstens een rijdend voertuig is betrokken. Of een verkeerssituatie waarbij een van de betrokkenen met opzet, wetende dat er een verkeersongeval heeft plaatsgevonden, de plaats van ongeval heeft verlaten zonder daarbij anderen de gelegenheid te bieden tot het vaststellen van diens identiteit."
    }else if (input$soort_crime == "Zedenmisdrijf"){
      "Hieronder vallen openbare schennis der eerbaarheid, verkrachting, aanranding, overige zedenmisdrijven, pornografie, seksueel misbruik (incest) afhankelijkheidsrelatie of wilsonbekwame, seksueel misbruik kinderen (geen incest), sexting, grooming"
    }else if (input$soort_crime == "Moord, doodslag"){
      "Hieronder vallen doodslag/moord, euthanasie, overige misdrijven tegen het leven, illegale abortus en behulpzaam bij zelfdoding."
    }else if (input$soort_crime == "Openlijk geweld (persoon)"){
      " Het openlijk in vereniging plegen van geweld tegen personen"
    }else if (input$soort_crime == "Bedreiging"){
      "Hieronder vallen bedreiging, overige misdrijven tegen de persoonlijke vrijheid, grijzeling/ontvoering en stalking."
    }else if (input$soort_crime == "Mishandeling"){
      "Iemand opzettelijk (zwaar) lichamelijk letsel toebrengen."
    }else if (input$soort_crime == "Straatroof"){
      "Het met geweld of onder bedreiging met geweld (voorafgegaan, vergezeld of gevolgd) wegnemen of afpersen van
        geld of goederen, gepleegd tegen een of meer personen op de openbare weg, m.u.v. geplande
        professionele/particuliere geld/waarde transporten. Hieronder vallen ook de berovingen gepleegd in een lift, galerij,
        binnenstraat, portiek of trapportaal."
    }else if (input$soort_crime == "Overval"){
      "Het met geweld of onder bedreiging van geweld (voorafgegaan, vergezeld of gevolgd), wegnemen of afpersen van
        enig goed, gepleegd tegen personen die zich in een woning of besloten pand of een professionele- en particuliere geld- en/of
        waardentransporten bevinden of op een gepland/georganiseerd (waarde-) transport."
    }else if (input$soort_crime == "Diefstallen (water)"){
      "De diefstal (ook d.m.v. braak, verbreking, inklimming, valse sleutel, valse order of vals kostuum) van voorwerpen of
        onderdelen vanaf of uit het vervoermiddel waarmee je je op of onder het water kunt verplaatsen of van een vervoermiddel waarmee je je op of onder het water kunt verplaatsen (niet zijnde een
        woonboot). Het vervoermiddel dient zich te bevinden op of aan het openbaar water of openbaar toegankelijke cq niet fysiek afgeschermde plaats. Eventueel vergezeld of gevolgd door (bedreiging met) geweld gericht tegen personen uit/vanaf/van een vervoermiddel waarmee je
        je op of onder het water kunt verplaatsen (niet zijnde een woonboot)"
    }else if (input$soort_crime == "Brand/ontploffing"){
      "Het opzettelijk in brand steken van een goed, een ontploffing teweegbrengen of een overstroming veroorzaken of het  opzettelijk met behulp van springstof een of meerdere mensen te verwonden of te doden of om objecten te
        beschadigen of te vernietigen."
    } else if (input$soort_crime == "Overige vermogensdelicten"){
      "Hieronder vallen diefstal in/uit andere gebouwen (eventueel met geweld), diefstal dier, verduistering, heling, chantage/afpersing, overige (eenvoudige) diefstal (met geweld)."
    } else if (input$soort_crime == "Mensenhandel"){
      "Iemand bewegen of ertoe brengen tot het verrichten van arbeid of seksuele handelingen met of voor een derde tegen
        betaling. Of iemand criminele activiteiten laten verrichten, of iemand onder dwang diens organen ter beschikking te laten stellen of laten verwijderen, of overige vormen van uitbuiting"
    } else if (input$soort_crime == "Drugs/drankoverlast"){
      "Overige drugsdelicten, met name in- en uitvoer van middelen genoemd in de Opiumwet."
    } else if (input$soort_crime == "Vernieling cq. zaakbeschadiging"){
      "Hieronder vallen vernieling van/aan auto, vernieling van/aan openbaar vervoer/abri, vernieling van/aan openbaar gebouw, vernieling overige objecten en openlijke geweldpleging tegen goederen"
    } else if (input$soort_crime == "Burengerucht (relatieproblemen)"){
      "Het overtreden van een opgelegd huisverbod, dat wil zeggen het zich bevinden op/in de locatie waarvoor het
        huisverbod geldt, waaronder het zoeken van contact, o.a. ook email en telefonisch contact"
    } else if (input$soort_crime == "Huisvredebreuk"){
      "Wederrechtelijk binnendringen in een woning of besloten lokaal of erf bij een ander in gebruik, of, wederrechtelijk
        aldaar vertoevende, zich niet op eerste vordering van de rechthebbende verwijderen uit die woning."
    } else if (input$soort_crime == "Diefstal/inbraak bedrijven en instellingen"){
      "Diefstal zonder of d.m.v. braak, verbreking, inklimming, valse sleutel, valse order of vals kostuum in/uit winkel die op dat
        moment niet voor publiek geopend was/een bedrijf of kantoor/een sportcomplex/een hotel of pension/een school. vergezeld of
        gevolgd door (bedreiging met) geweld gericht tegen personen in of uit het bedrijf/de instellingen."
    } else if (input$soort_crime == "Winkeldiefstal"){
      "Diefstal van uitgestalde, voor de verkoop bestemde goederen uit of nabij een winkel gedurende de openingstijden.Eventueel vergezeld of gevolgd door (bedreiging met) geweld gericht tegen personen, van uitgestalde,
        voor de verkoop bestemde goederen uit of nabij een winkel gedurende de openingstijden"
    } else if (input$soort_crime == "Inrichting Wet Milieubeheer"){
      "Alle strafbare handelingen met betrekking tot inrichtingen aangaande de Wet milieubeheer. Alle strafbare handelingen m.b.t. vuurwerk gerelateerde inrichtingen."
    } else if (input$soort_crime == "Bestrijdingsmiddelen"){
      "Het gebruik en toepassen van bestrijdingsmiddelen, in strijd met Bestrijdingsmiddelenwet/besluit. Of Het opgeslagen hebben/ het opgeslagen houden en het voorhanden hebben van bestrijdingsmiddelen in strijd met
        het Bestrijdingsmiddelenwet/besluit. "
    } else if (input$soort_crime == "Bodem"){
      "Hieronder vallen het op/in de bodem brengen van afvalstoffen, afval van drugslab, bodemverontreiniging en ontgrondingen."
    } else if (input$soort_crime == "Water"){
      "Hieronder vallen het verontreiningen of de onttrekking van oppervlaktewater en slootdemping"
    } else if (input$soort_crime == "Afval"){
      "Hieronder vallen misdaden met betrekking tot afvaltransport, afval verbranden, wrak (milieu), afvallozing in riool, afvalstoffen inzamelen, asbest."
    } else if (input$soort_crime == "Bouwstoffen"){
      "Hieronder vallen misdaden met betrekking tot bouw- en sloopafval, bouwstoffen op of in de bodem, bouwstoffen in oppervlaktewater."
    } else if (input$soort_crime == "Mest"){
      "Hieronder vallen misdaden met betrekking tot het uitrijden van mest, de opslag van mest en het vervoer van mest."
    } else if (input$soort_crime == "Transport gevaarlijke stoffen"){
      "Hieronder vallen misdaden met betrekking tot transport van gevaarlijke stoffen over de weg, over binnenwater, over de Rijn, over zee, over het spoor, door de lucht, en met betrekking tot CFK's en koelinstallaties."
    } else if (input$soort_crime == "Vuurwerk"){
      "Misdaden met betrekking tot het transport van vuurwerk, vuurwerkevenementen of het bezitten/vervaardigen/voorhanden hebben/afleveren van vuurwerk."
    } else if (input$soort_crime == "Bijzondere wetten"){
      "Hieronder vallen misdrijven van de wet op de kansspelen, de telecommunicatiewet, misdrijven die niet onder een specifieke klasse vallen en witwassen."
    } else if (input$soort_crime == "Leefbaarheid overig"){
      "Hieronder vallen bijtincidenten met dieren en lokaalvredebreuk."
    } else if (input$soort_crime == "Drugshandel"){
      "Het bezit, de handel en het vervaardigen van hard- en softdrugs."
    } else if (input$soort_crime == "Mensensmokkel"){
      "Het onwettig en georganiseerd smokkelen van mensen over internationale grenzen heen."
    } else if (input$soort_crime == "Wapenhandel"){
      "Het bezit en de handel van vuurwapens en overige wapens."
    } else if (input$soort_crime == "Kinderporno"){
      "Het verspreiden, openlijk tentoonstellen, vervaardigen, invoeren, doorvoeren, uitvoeren of in bezit hebben van een
        afbeelding van een seksuele gedraging, waarbij een minderjarig kind is betrokken of schijnbaar is betrokken."
    } else if (input$soort_crime == "Kinderprostitutie"){
      "Elk gebruik van een kind bij sexuele activiteiten tegen vergoeding of elke andere vorm van beloning."
    } else if (input$soort_crime == "Onder invloed (lucht)"){
      "Vliegen onder invloed van drugs/medicijnen/alcohol, het weigeren van een bloedproef of een vervangend (urine)onderzoek."
    } else if (input$soort_crime == "Lucht overig"){
      "Overtreding luchtvaartwet"
    } else if (input$soort_crime == "Onder invloed (water)"){
      "Varen onder invloed drugs/medicijnen/alcohol, weigeren ademanalyse/bloedproef/vervangend (urine)onderzoek."
    } else if (input$soort_crime == "Onder invloed (weg)"){
      "Rijden onder invloed van drugs/geneesmiddelen/alcohol, weigeren ademanalyse/bloedproef/vervangend (urine)onderzoek."
    } else if (input$soort_crime == "Weg overig"){
      "Hieronder vallen rijden tijdens een rijverbod, rijden terwijl rijbewijs is ingevorderd, rijden tijdens ontzegging rijbevoegdheid, rijden met ongeldig verklaard rijbewijs, joyriding, vals kenteken/valse kenteken platen en overige verkeersmisdrijven."
    } else if (input$soort_crime == "Aantasting openbare orde"){
      "Hieronder vallen de overige delicten openbare orde, wederspanningheid (verzet tegen ambtenaar in functie), niet voldoen aan bevel/vordering, overige misdrijven tegen het openbaar gezag."
    } else if (input$soort_crime == "Discriminatie"){
      "Elke vorm van onderscheid, elke uitsluiting, beperking of voorkeur, die ten doel heeft of ten gevolge kan hebben dat
        de erkenning, het genot of de uitoefening op voet van gelijkheid van de rechten van de mens en de fundamentele
        vrijheden op politiek, economisch, sociaal of cultureel terrein of op andere terreinen van het openbare leven, wordt
        teniet gedaan of aangetast."
    } else if (input$soort_crime == "Vreemdelingenzorg"){
      "Zich als ongewenst verklaarde vreemdeling in Nederland bevinden"
    } else if (input$soort_crime == "Maatschappelijke intergriteit (overig)"){
      "Hieronder vallen belediging en ontucht met dieren/dierenporno"
    } else if (input$soort_crime == "Cybercrime"){
      "Alle vormen van bezitsaantasting waarbij de computer zowel het middel als het doel is."
    } else if (input$soort_crime == "Horizontale fraude"){
      "Fraude die gericht is tegen burgers, bedrijven en financiele instellingen.Hieronder vallen fraude met betaalproducten, IE-fraude/namaakgoederen, identiteitsfraude, verzekeringsfraude of assurantiefraude, faillissementsfraude, krediet-, hypotheek- en depotfraude, acquisitiefraude, vastgoedfraude, fraude met kilometertellers, fraude in de zorg, fraude met online handel, voorschotfraude, telecomfraude, beleggingsfraude en overige horizontale fraude"
    } else if (input$soort_crime == "Verticale fraude"){
      "Fraude die gericht is tegen de overheid. Hieronder vallen uitkeringsfraude, subsidiefraude en overige verticale fraude"
    } else if (input$soort_crime == "Overige fraude"){
      "Hieronder vallen vals geld maken, vals geld uitgeven en een valse aangifte."
    }
  }) 
  
  
}) # Shiny server

