# Load libraries
library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(htmltools)

# Load data
gemeenten <- readRDS("../Data/gemeenten.rds")
wijken <- readRDS("../Data/wijken.rds")
buurten <- readRDS("../Data/buurten.rds")
postcodes_final <- readRDS("../Data/postcodes_final.rds")
full_data <- readRDS("../Data/full_data.rds")
ongevallen <- readRDS("../Data/ongevallen_W84.rds")
all_polygons <- full_data %>% 
  select(BU_CODE, BU_NAAM, WK_CODE, WK_NAAM, GM_CODE, GM_NAAM, POSTCODE, geometry, centroid, Niveau, CODE, NAAM, centroidx, centroidy)
intersection <- readRDS("../Data/intersection.rds")
  
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
        }else if (input$vergelijkbaar1 == "Opleidingsniveau"){
          opleiding_num <- df[df$GM_NAAM == input$gemeente1, 'opleidingsgroep']
          comparable_df <- df[df$opleidingsgroep == opleiding_num, ]
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
        }else if (input$vergelijkbaar2 == "Opleidingsniveau"){
          opleiding_num <- df[df$WK_NAAM == input$wijken2 & df$GM_NAAM == input$gemeente2, 'opleidingsgroep']
          if(is.na(opleiding_num)){
            comparable_df <- df[df$Niveau == input$niveau,]
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
            paste0(cuts[-n], " &ndash; ", cuts[-1])
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
    
    #Creates box for staafdiagram only when one of the subthemes is selected that has one
    output[["box_staafdiagram"]] <- renderUI({
      
      #subthemes that have data available on the count inside a radius
      subthemes_count <- c("Huisartsenpraktijk","Ziekenhuis", "Supermarkt", "Overige dagelijkse levensmiddelen", "Warenhuis",
                     "Café", "Cafetaria", "Restaurant", "Hotel", "Kinderdagverblijf", "Buitenschoolse opvang", "Basisschool",
                     "Voortgezet onderwijs", "VMBO school", "HAVO/VWO school", "Bioscoop", "Attractie", "Podiumkunsten", "Museum")
      #if the selected subthemes is in these subthemes with count, show the "staafdiagram" box
      if(input$subthema %in% subthemes_count){
        box(title = "Staafdiagram", width = 4, status = "warning", solidHeader = T,
            "Aantallen van het gekozen subthema binnen een bepaalde straal, voor het geselecteerde gebied (roze) en andere vergelijkbare gebieden (blauw).",
            #"Hier komt een staafdiagram om je wijk te vergelijken met het gemiddelde van vergelijkbare wijken",
            plotOutput("plot_variable"))
      }
    })
    
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
    
    # ONGEVALLEN/TRAFFIC INCIDENTS
    # Make selection dependent on previous input
    observeEvent(input$gemeente22, {
      updateSelectInput(session, 'wijken22',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente22]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$gemeente23, {
      updateSelectInput(session, 'wijken23',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente23]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$wijken23,{
      updateSelectInput(session, 'buurten23',
                        choices = unique(postcodes_final$buurtnaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente23 & 
                                                                         postcodes_final$wijknaam2020==input$wijken23]))       # Only display buurten that are in the selected wijk
    })
    
    # REACTION ON ACTION 2
    # Create input dataset for further analysis
    reaction2 <- eventReactive(input$action2, {
      sf_use_s2(F)
      polygons_niveau <- all_polygons %>% filter(Niveau == input$niveau2)  # Only polygons on selected niveau (gemeente, wijk, buurt)
      ongevallen_year <- ongevallen %>% filter(JAAR_VKL == input$jaar)     # Only ongevallen/incidents for selected year
      intersection <- intersection %>% filter(Niveau == input$niveau2 & 
                                                JAAR_VKL == input$jaar)    # Precalculated intersection filtered on niveau and year of input
      
      df_intersection <- as.data.frame(intersection) 
      
      if (input$niveau2 == "Gemeenten"){
        pol_select <- polygons_niveau %>% filter(GM_NAAM == input$gemeente21)
        incidents_count_niveau <- df_intersection %>% 
          group_by(GM_NAAM) %>% 
          count()
        number_incidents <- incidents_count_niveau %>% filter(GM_NAAM == input$gemeente21)
        intersection_select <- intersection %>% filter(GM_NAAM == input$gemeente21 &
                                                         Niveau == "Gemeenten")
      } else if(input$niveau2 == "Wijken"){
        pol_select <- polygons_niveau %>% filter(GM_NAAM == input$gemeente22 & 
                                                   WK_NAAM == input$wijken22)
        incidents_count_niveau <- df_intersection %>% 
          group_by(WK_NAAM, GM_NAAM) %>% 
          count()
        number_incidents <- incidents_count_niveau %>% filter(GM_NAAM == input$gemeente22 & 
                                                                WK_NAAM == input$wijken22)
        intersection_select <- intersection %>% filter(GM_NAAM == input$gemeente22 &
                                                         WK_NAAM == input$wijken22 &
                                                         Niveau == "Wijken")
      } else if (input$niveau2 == "Buurten"){
        pol_select <- polygons_niveau %>% filter(GM_NAAM == input$gemeente23 & 
                                                   WK_NAAM == input$wijken23 & 
                                                   BU_NAAM == input$buurten23)
        incidents_count_niveau <- df_intersection %>% 
          group_by(BU_NAAM, WK_NAAM, GM_NAAM) %>% 
          count()
        number_incidents <- incidents_count_niveau %>% filter(GM_NAAM == input$gemeente23 &
                                                                WK_NAAM == input$wijken23 &
                                                                BU_NAAM == input$buurten23)   
        intersection_select <- intersection %>% filter(GM_NAAM == input$gemeente23 &
                                                         WK_NAAM == input$wijken23 &
                                                         BU_NAAM == input$buurten23 &
                                                         Niveau == "Buurten")
      }
      
      number_incidents <- number_incidents$n
      
      list_ongevallen_return <- list("ongevallen_year" = ongevallen_year,          # Dataset with points of incidents in selected year of all polygons to plot
                                     "polygons_niveau" = polygons_niveau,          # Dataset with all polygons of selected niveau to plot on map
                                     "pol_select" = pol_select,                    # Dataset with selected polygon/area 
                                     "intersection_select" = intersection_select,  # All point data of selected area in selected year
                                     "number_incidents" = number_incidents)        # Number of incidents in selected area and year)          
                                      
      return(list_ongevallen_return)   
    })
    
    # Output number of incidents in selected area
    output$number_incidents <- renderValueBox(
      valueBox(reaction2()$number_incidents, "ongevallen in het door u geselecteerde gebied", 
               icon = icon("car-crash", class = "fa-solid fa-car-burst", lib = "font-awesome"), 
               color = "red") # valuebox
    ) # rendervaluebox
    
    # General trend in selected area
    input_trend <- eventReactive(input$action2, {
      if (input$niveau2 == "Gemeenten"){
        area <- intersection %>% filter(GM_NAAM == input$gemeente21 & 
                                          Niveau == "Gemeenten")
      } else if(input$niveau2 == "Wijken"){
        area <- intersection %>% filter(GM_NAAM == input$gemeente22 & 
                                          WK_NAAM == input$wijken22 & 
                                          Niveau == "Wijken")
      } else if (input$niveau2 == "Buurten"){
        area <- intersection %>% filter(GM_NAAM == input$gemeente23 & 
                                          WK_NAAM == input$wijken23 & 
                                          BU_NAAM == input$buurten23 & 
                                          Niveau == "Buurten")
      }
      return(area)
    })
    
    # Trend line of accidents in selected area over the years
    output$general_trend <- renderPlot({
      input_trend() %>% count(JAAR_VKL) %>%
        ggplot() +
        geom_line(aes(x = as.factor(JAAR_VKL), y = n, group = 1), color = "Red") +
        labs(title = "Aantal ongevallen per jaar",
             x = "Jaar",
             y = "Aantal")+
        theme_classic()
    })
    
    # REACTION ON ACTION 3
    # Reaction on subthema input with the correct dataset
    reaction3 <- eventReactive(input$action3, {
      intersection_select <- reaction2()$intersection_select   # Points in selected polygon (filtered on niveau, naam and year)
      if (input$subthema2 == "WGD_CODE_1"){
        color_incidents <- colorFactor(topo.colors(6), intersection_select$WGD_CODE_1)   # Create color palette for categoric variable
        subthema <- intersection_select$WGD_CODE_1                                       # Set subthema that can be used for color map
        subthema_char <-"Weersgesteldheid"                                               # Character of subthema for title
        
        bar_chart <- intersection_select %>%                                             # Bar chart for tab1 in dashboard
          count(WGD_CODE_1) %>% 
          ggplot() +
          geom_col(aes(x = WGD_CODE_1, y=n, fill = WGD_CODE_1), show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45))
        
        pie_chart <- intersection_select %>%                                             # Pie chart for tab2 in dashboard
          count(WGD_CODE_1) %>% 
          ggplot(aes(x = "", y = n, fill = WGD_CODE_1)) +
          geom_bar(stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char)
        
        trend_theme <- input_trend() %>% count(JAAR_VKL, WGD_CODE_1) %>% 
          ggplot(aes(x = as.factor(JAAR_VKL), y = n, group = WGD_CODE_1, color = WGD_CODE_1))+
          geom_line() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = "Jaar",
               y = "Aantal",
               color = subthema_char)+
          theme_classic()

      } else if (input$subthema2 == "AP3_OMS"){
        color_incidents <- colorFactor(topo.colors(3), intersection_select$AP3_OMS)
        subthema <- intersection_select$AP3_OMS 
        subthema_char <-"Afloop"
        
        bar_chart <- intersection_select %>% 
          count(AP3_OMS) %>% 
          ggplot() +
          geom_col(aes(x = AP3_OMS, y=n, fill = AP3_OMS), show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          theme(axis.text.x = element_text(angle = 45))
        
        pie_chart <- intersection_select %>%
          count(AP3_OMS) %>% 
          ggplot(aes(x = "", y = n, fill = AP3_OMS)) +
          geom_bar(stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char)
        
        trend_theme <- input_trend() %>% count(JAAR_VKL, AP3_OMS) %>% 
          ggplot(aes(x = as.factor(JAAR_VKL), y = n, group = AP3_OMS, color = AP3_OMS))+
          geom_line() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = "Jaar",
               y = "Aantal",
               color = subthema_char)+
          theme_classic()
        
      } else if (input$subthema2 == "UITGPOS1"){
        color_incidents <- colorFactor(topo.colors(8), intersection_select$UITGPOS1)
        subthema <- intersection_select$UITGPOS1 
        subthema_char <-"Uitgangspositie"
        
        bar_chart <- intersection_select %>% 
          count(UITGPOS1) %>% 
          ggplot() +
          geom_col(aes(x = UITGPOS1, y=n, fill = UITGPOS1), show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          theme(axis.text.x = element_text(angle = 45))
        
        pie_chart <- intersection_select %>%
          count(UITGPOS1) %>% 
          ggplot(aes(x = "", y = n, fill = UITGPOS1)) +
          geom_bar(stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char)
        
        trend_theme <- input_trend() %>% count(JAAR_VKL, UITGPOS1) %>% 
          ggplot(aes(x = as.factor(JAAR_VKL), y = n, group = UITGPOS1, color = UITGPOS1))+
          geom_line() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = "Jaar",
               y = "Aantal",
               color = subthema_char)+
          theme_classic()
        
      } else if (input$subthema2 == "VOORGBEW"){
        color_incidents <- colorFactor(topo.colors(11), intersection_select$VOORGBEW)
        subthema <- intersection_select$VOORGBEW 
        subthema_char <-"Voorgenomen beweging"
        
        bar_chart <- intersection_select %>% 
          count(VOORGBEW) %>% 
          ggplot() +
          geom_col(aes(x = VOORGBEW, y=n, fill = VOORGBEW), show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          theme(axis.text.x = element_text(angle = 45))
        
        pie_chart <- intersection_select %>%
          count(VOORGBEW) %>% 
          ggplot(aes(x = "", y = n, fill = VOORGBEW)) +
          geom_bar(stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char)
        
        trend_theme <- input_trend() %>% count(JAAR_VKL, VOORGBEW) %>% 
          ggplot(aes(x = as.factor(JAAR_VKL), y = n, group = VOORGBEW, color = VOORGBEW))+
          geom_line() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = "Jaar",
               y = "Aantal",
               color = subthema_char)+
          theme_classic()
        
      } else if (input$subthema2 == "BWG_OMS"){
        color_incidents <- colorFactor(topo.colors(9), intersection_select$BWG_OMS)
        subthema <- intersection_select$BWG_OMS 
        subthema_char <-"Beweging ten gevolge van ongeval"
        
        bar_chart <- intersection_select %>% 
          count(BWG_OMS) %>% 
          ggplot() +
          geom_col(aes(x = BWG_OMS, y=n, fill = BWG_OMS), show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45))
        
        pie_chart <- intersection_select %>%
          count(BWG_OMS) %>% 
          ggplot(aes(x = "", y = n, fill = BWG_OMS)) +
          geom_bar(stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char)
        
        trend_theme <- input_trend() %>% count(JAAR_VKL, BWG_OMS) %>% 
          ggplot(aes(x = as.factor(JAAR_VKL), y = n, group = BWG_OMS, color = BWG_OMS))+
          geom_line() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = "Jaar",
               y = "Aantal",
               color = subthema_char)+
          theme_classic()
        
      } else if (input$subthema2 == "OTE_OMS"){
        color_incidents <- colorFactor(topo.colors(23), intersection_select$OTE_OMS)
        subthema <- intersection_select$OTE_OMS 
        subthema_char <-"Objecttype"
        
        bar_chart <- intersection_select %>% 
          count(OTE_OMS) %>% 
          ggplot() +
          geom_col(aes(x = OTE_OMS, y=n, fill = OTE_OMS), show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          theme(axis.text.x = element_text(angle = 45))
        
        pie_chart <- intersection_select %>%
          count(OTE_OMS) %>% 
          ggplot(aes(x = "", y = n, fill = OTE_OMS)) +
          geom_bar(stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char)
        
        trend_theme <- input_trend() %>% count(JAAR_VKL, OTE_OMS) %>% 
          ggplot(aes(x = as.factor(JAAR_VKL), y = n, group = OTE_OMS, color = OTE_OMS))+
          geom_line() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = "Jaar",
               y = "Aantal",
               color = subthema_char)+
          theme_classic()
        
      } else if (input$subthema2 == "WSE_OMS"){
        color_incidents <- colorFactor(topo.colors(9), intersection_select$WSE_OMS)
        subthema <- intersection_select$WSE_OMS 
        subthema_char <-"Wegsituatie"
        
        bar_chart <- intersection_select %>% 
          count(WSE_OMS) %>% 
          ggplot() +
          geom_col(aes(x = WSE_OMS, y=n, fill = WSE_OMS), show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          theme(axis.text.x = element_text(angle = 45))
        
        pie_chart <- intersection_select %>%
          count(WSE_OMS) %>% 
          ggplot(aes(x = "", y = n, fill = WSE_OMS)) +
          geom_bar(stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char)
        
        trend_theme <- input_trend() %>% count(JAAR_VKL, WSE_OMS) %>% 
          ggplot(aes(x = as.factor(JAAR_VKL), y = n, group = WSE_OMS, color = WSE_OMS))+
          geom_line() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = "Jaar",
               y = "Aantal",
               color = subthema_char)+
          theme_classic()
        
      } else if (input$subthema2 == "BZD_ID_VM1"){
        color_incidents <- colorFactor(topo.colors(9), intersection_select$BZD_ID_VM1)
        subthema <- intersection_select$BZD_ID_VM1 
        subthema_char <-"Bijzonderheid verkeersmaatregel"
        
        bar_chart <- intersection_select %>% 
          count(BZD_ID_VM1) %>% 
          ggplot() +
          geom_col(aes(x = BZD_ID_VM1, y=n, fill = BZD_ID_VM1), show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          theme(axis.text.x = element_text(angle = 45))
        
        pie_chart <- intersection_select %>%
          count(BZD_ID_VM1) %>% 
          ggplot(aes(x = "", y = n, fill = BZD_ID_VM1)) +
          geom_bar(stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char)
        
        trend_theme <- input_trend() %>% count(JAAR_VKL, BZD_ID_VM1) %>% 
          ggplot(aes(x = as.factor(JAAR_VKL), y = n, group = BZD_ID_VM1, color = BZD_ID_VM1))+
          geom_line() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = "Jaar",
               y = "Aantal",
               color = subthema_char)+
          theme_classic()
        
        
      } else if (input$subthema2 == "BZD_ID_IF1"){
        color_incidents <- colorFactor(topo.colors(9), intersection_select$BZD_ID_IF1)
        subthema <- intersection_select$BZD_ID_IF1 
        subthema_char <-"Bijzonderheid infrastructuur"
        
        bar_chart <- intersection_select %>% 
          count(BZD_ID_IF1) %>% 
          ggplot() +
          geom_col(aes(x = BZD_ID_IF1, y=n, fill = BZD_ID_IF1), show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45))
        
        pie_chart <- intersection_select %>%
          count(BZD_ID_IF1) %>% 
          ggplot(aes(x = "", y = n, fill = BZD_ID_IF1)) +
          geom_bar(stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char)
        
        trend_theme <- input_trend() %>% count(JAAR_VKL, BZD_ID_IF1) %>% 
          ggplot(aes(x = as.factor(JAAR_VKL), y = n, group = BZD_ID_IF1, color = BZD_ID_IF1))+
          geom_line() +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = "Jaar",
               y = "Aantal",
               color = subthema_char)+
          theme_classic()
        
      } 

      list_return <- list("color_incidents" = color_incidents,         # Color palette for categorical variable
                          "subthema" = subthema,                       # Name of subthema variable for the function 
                          "intersection_select" = intersection_select, # All points of incidents in selected area for selected year 
                          "subthema_char" = subthema_char,             # Character: title of legend
                          "pie_chart" = pie_chart,                     # Pie chart with ggplot reactive on subthema
                          "bar_chart" = bar_chart,                     # Bar chart with ggplot reactive on subthema
                          "trend_theme" = trend_theme                  # Trend line with ggplot reactive on subthema
                          )                     
      
      return(list_return)
    }) # Function reaction 3
    
    # New map with coloring of selected variable
    color_map_incidents <- function(df,                # Dataset of points in selected area and year
                                    subthema,          # Subthema selected in input
                                    subthema_char,     # Name for subthema in legend title
                                    color_incidents){  # Color palette
      map <- leaflet(df) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(radius = 5,
                         color = ~color_incidents(subthema),
                         label = ~subthema) %>% 
        setView(lng = reaction2()$pol_select$centroidx,     # Make sure that the zoom is concentrated on selected area
                lat = reaction2()$pol_select$centroidy,
                zoom = 13) %>%
        addPolylines(data = reaction2()$pol_select,
                     stroke = T,
                     weight = 3) %>%
        addLegend(pal = color_incidents, 
                  values = ~subthema,
                  title = subthema_char)
      
      return(map)
    } # Color_map_incidents function

    # Map output of the incidents colored by the selected variable
    output$map_color_incidents <- renderLeaflet({
      color_map_incidents(reaction3()$intersection_select,
                          reaction3()$subthema,
                          reaction3()$subthema_char,
                          reaction3()$color_incidents)
    })
    
    # Bar chart 
    output$bar_chart <- renderPlot({
      reaction3()$bar_chart
    })
    
    # Pie chart
    output$pie_chart <- renderPlot({
      reaction3()$pie_chart
    })
    
    # Trend line selected subtheme
    output$trend_theme <- renderPlot({
      reaction3()$trend_theme
    })
      
})

