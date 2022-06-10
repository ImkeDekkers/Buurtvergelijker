library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(htmltools)

gemeenten <- readRDS("../Data/gemeenten.rds")
wijken <- readRDS("../Data/wijken.rds")
buurten <- readRDS("../Data/buurten.rds")
postcodes_final <- readRDS("../Data/postcodes_final.rds")
full_data <- readRDS("../Data/full_data.rds")
gezondheid_all <-readRDS("../Data/gezondheid_all.rds")
source("../SimilarAgeDistribution.R")
source("../HealthPlots.R")

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
    
    
    ######START GEZONDHEID
    
    
    
    # Make selection dependent on previous input
    observeEvent(input$gemeente2_gez, {
      updateSelectInput(session, 'wijken2_gez',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente2_gez]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$gemeente3_gez, {
      updateSelectInput(session, 'wijken3_gez',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente3_gez]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$wijken3_gez,{
      updateSelectInput(session, 'buurten3_gez',
                        choices = unique(postcodes_final$buurtnaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente3_gez & 
                                                                         postcodes_final$wijknaam2020==input$wijken3_gez]))       # Only display buurten that are in the selected wijk
    })
    
    observeEvent(input$thema_gez, {
      if (input$thema_gez == "Gezondheid en beperkingen") {
        updateSelectInput(session, 'subthema_gez', 
                          choices = c("Zeer goede of goede gezondheid (%)", "Langdurige aandoening (%)", 
                                      "Langdurige ziekte en beperkt (%)", "Beperking", "Beperkt vanwege gezondheid",
                                      "Risico op angststoornis of depressie", "(heel) veel stress (%)"))
      }else if (input$thema_gez == "Leefstijl") {
        updateSelectInput(session, 'subthema_gez', 
                          choices = c("Voldoet aan beweegrichtlijn (%)", "Wekelijkse sporters (%)", "Gewicht",
                                      "Rokers (%)", "Alcoholgebruik", "Lopen/fietsen naar school of werk"))
      }else if (input$thema_gez == "Participatie en omgeving") {
        updateSelectInput(session, 'subthema_gez', 
                          choices = c("Matig tot veel regie over eigen leven (%)", "Eenzaamheid", "Mantelzorger (%)", "Vrijwilligerswerk (%)", 
                                      "Ernstige geluidhinder door buren (%)", "Moeite met rondkomen (%)"))
      }
    })
    
    #Change input choices for histogram depending on subtheme
    observeEvent(input$action_thema_gez, {
      if (input$subthema_gez == "Beperking") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Lichamelijke beperking (%)", "Beperking in horen (%)", "Beperking in zien (%)","Beperking in bewegen (%)"))
      }else if(input$subthema_gez == "Beperkt vanwege gezondheid") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Beperkt vanwege gezondheid (%)","Ernstig beperkt vanwege gezondheid (%)"))
      }else if(input$subthema_gez == "Risico op angststoornis of depressie") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Matig tot hoog risico op angststoornis of depressie (%)","Hoog risico op angststoornis of depressie (%)"))
      }else if(input$subthema_gez == "Gewicht") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Ondergewicht (%)","Normaal gewicht (%)","Overgewicht (%)","Ernstig overgewicht (%)"))
      }else if(input$subthema_gez == "Alcoholgebruik") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Voldoet aan alcoholrichtlijn (%)","Drinkers (%)","Zware drinkers (%)","Overmatige drinkers (%)"))
      }else if(input$subthema_gez == "Lopen/fietsen naar school of werk") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)"))
      }else if(input$subthema_gez == "Eenzaamheid") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)"))
      }
    })
    
    #Change input choices for line plot depending on subtheme
    observeEvent(input$action_thema_gez, {
      if (input$subthema_gez == "Beperking") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Lichamelijke beperking (%)", "Beperking in horen (%)", "Beperking in zien (%)","Beperking in bewegen (%)"))
      }else if(input$subthema_gez == "Beperkt vanwege gezondheid") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Beperkt vanwege gezondheid (%)","Ernstig beperkt vanwege gezondheid (%)"))
      }else if(input$subthema_gez == "Risico op angststoornis of depressie") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Matig tot hoog risico op angststoornis of depressie (%)","Hoog risico op angststoornis of depressie (%)"))
      }else if(input$subthema_gez == "Gewicht") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Ondergewicht (%)","Normaal gewicht (%)","Overgewicht (%)","Ernstig overgewicht (%)"))
      }else if(input$subthema_gez == "Alcoholgebruik") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Voldoet aan alcoholrichtlijn (%)","Drinkers (%)","Zware drinkers (%)","Overmatige drinkers (%)"))
      }else if(input$subthema_gez == "Lopen/fietsen naar school of werk") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)"))
      }else if(input$subthema_gez == "Eenzaamheid") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)"))
      }
    })
    
    #Change input choices for staafdiagram depending on subtheme
    observeEvent(input$action_thema_gez, {
      if (input$subthema_gez == "Beperking") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Lichamelijke beperking (%)", "Beperking in horen (%)", "Beperking in zien (%)","Beperking in bewegen (%)"))
      }else if(input$subthema_gez == "Beperkt vanwege gezondheid") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Beperkt vanwege gezondheid (%)","Ernstig beperkt vanwege gezondheid (%)"))
      }else if(input$subthema_gez == "Risico op angststoornis of depressie") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Matig tot hoog risico op angststoornis of depressie (%)","Hoog risico op angststoornis of depressie (%)"))
      }else if(input$subthema_gez == "Gewicht") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Ondergewicht (%)","Normaal gewicht (%)","Overgewicht (%)","Ernstig overgewicht (%)"))
      }else if(input$subthema_gez == "Alcoholgebruik") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Voldoet aan alcoholrichtlijn (%)","Drinkers (%)","Zware drinkers (%)","Overmatige drinkers (%)"))
      }else if(input$subthema_gez == "Lopen/fietsen naar school of werk") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)"))
      }else if(input$subthema_gez == "Eenzaamheid") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)"))
      }
    })
    
    #Change input choices for map with variable depending on subtheme
    observeEvent(input$action_thema_gez, {
      if (input$subthema_gez == "Beperking") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Lichamelijke beperking (%)", "Beperking in horen (%)", "Beperking in zien (%)","Beperking in bewegen (%)"))
      }else if(input$subthema_gez == "Beperkt vanwege gezondheid") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Beperkt vanwege gezondheid (%)","Ernstig beperkt vanwege gezondheid (%)"))
      }else if(input$subthema_gez == "Risico op angststoornis of depressie") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Matig tot hoog risico op angststoornis of depressie (%)","Hoog risico op angststoornis of depressie (%)"))
      }else if(input$subthema_gez == "Gewicht") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Ondergewicht (%)","Normaal gewicht (%)","Overgewicht (%)","Ernstig overgewicht (%)"))
      }else if(input$subthema_gez == "Alcoholgebruik") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Voldoet aan alcoholrichtlijn (%)","Drinkers (%)","Zware drinkers (%)","Overmatige drinkers (%)"))
      }else if(input$subthema_gez == "Lopen/fietsen naar school of werk") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)"))
      }else if(input$subthema_gez == "Eenzaamheid") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)"))
      }
    })
    
    #Dividing the subthemes about health between normal and special (having multiple closely related variables)
    Normal <- c("Zeer goede of goede gezondheid (%)", "Langdurige aandoening (%)", 
                "Langdurige ziekte en beperkt (%)",  "(heel) veel stress (%)",
                "Voldoet aan beweegrichtlijn (%)", "Wekelijkse sporters (%)", 
                "Rokers (%)", "Matig tot veel regie over eigen leven (%)", 
                "Mantelzorger (%)", "Vrijwilligerswerk (%)", 
                "Ernstige geluidhinder door buren (%)","Moeite met rondkomen (%)")
    Special <- c("Beperking", "Gewicht", "Alcoholgebruik", "Lopen/fietsen naar school of werk", "Eenzaamheid", 
                 "Risico op angststoornis of depressie", "Beperkt vanwege gezondheid")

    
    #make used GEZONDHEID data reactive on the selected niveau
    Gez_datasetInput <- eventReactive(input$action_gez,{
      df <- as.data.frame(gezondheid_all)
      df <- df[df$Niveau == input$niveau_gez,]
      if(input$niveau_gez == 'Gemeenten'){
        if(input$vergelijkbaar1_gez == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
          stedelijkheid_num <- stedelijkheid_num[1]
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
        }else if (input$vergelijkbaar1_gez == "Inkomensniveau"){
          inkomen_num <- df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(`inkomengroep`)         
          inkomen_num<-inkomen_num[1]
          comparable_df <- df[df$inkomengroep == inkomen_num, ]
        }else if(input$vergelijkbaar1_gez == "Nederland"){
          comparable_df <- df
        }else if(input$vergelijkbaar1_gez == "age_distribution"){
          comparable_df <- df
        }
        selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(CODE)
        comparable_df$selected_area_code <- selected_area_code[1]
        if(input$vergelijkbaar1_gez=="age_distribution"){
          comparable_df <- similar_age(comparable_df, input$niveau_gez)
          selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(CODE)
          comparable_df$selected_area_code <- selected_area_code[1]
        }
        selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(NAAM)
        comparable_df$selected_area_label <- selected_area_label[1]
        selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente1)
        selected_polygon <- selected_polygon[1,]
        row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente1_gez)
        row_num_selected <-row_num_selected[1]
      }else if(input$niveau_gez == 'Wijken'){
        if(input$vergelijkbaar2_gez == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df %>% filter(WK_NAAM == input$wijken2_gez & GM_NAAM == input$gemeente2_gez) %>% pull(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
          stedelijkheid_num <- stedelijkheid_num[1]
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
          comparable_df <- comparable_df %>% drop_na(CODE)
          output$error_vergelijkbaarheid_gez <- renderText(
            print("")
          )
        }else if (input$vergelijkbaar2_gez == "Inkomensniveau"){
          inkomen_num <-df %>% filter(WK_NAAM == input$wijken2_gez & GM_NAAM == input$gemeente2_gez) %>% pull(`inkomengroep`)
          inkomen_num<-inkomen_num[1]
          if(is.na(inkomen_num)){
            comparable_df <- df[df$Niveau == input$niveau_gez,]
            output$error_vergelijkbaarheid_gez <- renderText(
              print("Let op, door een missende waarde van het inkomensniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
            )
          }else{
            comparable_df <- df[df$inkomengroep == inkomen_num, ]
            comparable_df <- comparable_df %>% drop_na(CODE)
            output$error_vergelijkbaarheid_gez <- renderText(
              print("")
            )
          }
        }else if(input$vergelijkbaar2_gez == "Nederland"){
          comparable_df <- df
          output$error_vergelijkbaarheid_gez <- renderText(
            print("")
          )
        }else if(input$vergelijkbaar2_gez == "age_distribution"){
          comparable_df <- df
        }
        selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente2_gez & WK_NAAM == input$wijken2_gez) %>%pull(CODE)
        comparable_df$selected_area_code <- selected_area_code[1]
        if(input$vergelijkbaar2_gez == "age_distribution"){
          comp_df <- similar_age(comparable_df, input$niveau_gez)
          if(any(is.na(comp_df$`Personen 0 tot 15 jaar (%)`))){
            comparable_df <- df 
            output$error_vergelijkbaarheid_gez <- renderText(
              print("Let op, door missende gegevens over de leeftijdsopbouw voor uw wijk, wordt er nu met heel Nederland vergeleken.")
            )
          }else{
            comparable_df <- comp_df
            output$error_vergelijkbaarheid_gez <- renderText(
              print("")
            )
          }
          selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente2_gez & WK_NAAM == input$wijken2_gez) %>%pull(CODE)
          comparable_df$selected_area_code <- selected_area_code[1]
        }
        selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente2_gez & WK_NAAM == input$wijken2_gez) %>%pull(NAAM)
        comparable_df$selected_area_label <- selected_area_label[1]
        selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente2_gez & WK_NAAM == input$wijken2_gez)
        selected_polygon <- selected_polygon[1,]
        row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente2_gez & comparable_df$WK_NAAM == input$wijken2_gez)
        row_num_selected <-row_num_selected[1]
      }else if(input$niveau_gez == 'Buurten'){
        if(input$vergelijkbaar3_gez == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df%>% filter(BU_NAAM==input$buurten3_gez & GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez) %>% pull(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
          stedelijkheid_num <- stedelijkheid_num[1]
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
          comparable_df <- comparable_df %>% drop_na(CODE)
          output$error_vergelijkbaarheid_gez <- renderText(
            print("")
          )
        } else if(input$vergelijkbaar3_gez == "Nederland"){
          comparable_df <- df
          output$error_vergelijkbaarheid_gez <- renderText(
            print("")
          )
        }else if(input$vergelijkbaar3_gez == "age_distribution"){
          comparable_df <- df
        }
        selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez & BU_NAAM == input$buurten3_gez) %>%pull(CODE)
        comparable_df$selected_area_code <- selected_area_code[1]
        if(input$vergelijkbaar3_gez == "age_distribution"){
          comp_df <- similar_age(comparable_d, input$niveau_gezf)
          if(any(is.na(comp_df$`Personen 0 tot 15 jaar (%)`))){
            comparable_df <- df 
            output$error_vergelijkbaarheid_gez <- renderText(
              print("Let op, door missende gegevens over de leeftijdsopbouw voor uw buurt, wordt er nu met heel Nederland vergeleken.")
            )
          }else{
            comparable_df <- comp_df
            output$error_vergelijkbaarheid_gez <- renderText(
              print("")
            )
          }
          selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez & BU_NAAM == input$buurten3_gez) %>%pull(CODE)
          comparable_df$selected_area_code <- selected_area_code[1]
        }
        selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez & BU_NAAM == input$buurten3_gez) %>%pull(NAAM)
        comparable_df$selected_area_label <- selected_area_label[1]
        selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez & BU_NAAM == input$buurten3_gez)
        selected_polygon <- selected_polygon[1,]
        row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente3_gez & comparable_df$WK_NAAM == input$wijken3_gez & comparable_df$BU_NAAM == input$buurten3_gez)
        row_num_selected <-row_num_selected[1]
        
      }
      comparable_df$centroidxx <- comparable_df[row_num_selected, 'centroidx']
      comparable_df$centroidyy <- comparable_df[row_num_selected, 'centroidy']
      dataset <- st_as_sf(comparable_df)
      
      list_return <- list("dataset" = dataset, "selected_polygon" = selected_polygon)
      return(list_return)
    })
    
    # Get information about selected area in table
    output$info_area_gez <- renderTable({
      df <- as.data.frame(Gez_datasetInput()$dataset)
      df <- df[df$Perioden=="2020",]
      df <- df %>% drop_na(Perioden)
      selected_area_code <- df[1, "selected_area_code"]
      df <- df[df$CODE==selected_area_code,]
      df <- df %>%
        select(c("Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)", "inkomengroep"))
      df <- rename(df, "Stedelijkheid"= `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
      df[1,]
    })
    
    # Create map to point to the selected location and comparable polygons
    output$prime_map2 <- renderLeaflet({
      data<-Gez_datasetInput()$dataset
      data <- data[!duplicated(data[ , c("CODE")]), ]
      leaflet(data) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(color = "navy", weight = 1, 
                    highlightOptions = highlightOptions(color = "black", 
                                                        weight = 2),
                    label = ~htmlEscape(data$NAAM)) %>% 
        addAwesomeMarkers(lng = data$centroidxx,
                          lat = data$centroidyy,
                          icon = iconblue) 
    })
    
    #Selected subtheme
    selected_subtheme_gez <- eventReactive(input$action_thema_gez,{
      subtheme <- input$subthema_gez
      return(subtheme)
    })
    
    #creates a barplot of the selected area and the mean of comparable areas for all age classes
    #Uses function plot_gez from HealthPlots file
    output$gez_plot <- renderPlot({
      subtheme <- selected_subtheme_gez()
      plot_gez(Gez_datasetInput()$dataset, input$categorie_staaf, subtheme)
    })
    

    #Creates a line chart of the selected area and the mean of comparable areas
    #Uses function line_plot_gez from HealthPlots file
    output$gez_line_plot <- renderPlot({
      subtheme <- selected_subtheme_gez()
      line_plot_gez(Gez_datasetInput()$dataset, subtheme, input$norm_age_line, input$spec_age_line, input$categorie_line)
    })
    
    #Creates a histogram of the selected subtheme using the comparable areas
    #Uses function histogram from HealthPlots file
    output$gez_hist <- renderPlot({
      subtheme <- selected_subtheme_gez()
      histogram(Gez_datasetInput()$dataset, subtheme, input$norm_age_hist, input$categorie_hist, input$spec_age_hist)
    })
    
    #Creates a barplot per categorie if selected subtheme contains multiple variables
    #Uses function staaf_categorie from HealthPlots file
    output$staaf_cat <- renderPlot({
      subtheme <- selected_subtheme_gez()
      staaf_categorie(Gez_datasetInput()$dataset, subtheme, input$spec_age_cat)
    })
    
    #Creates a plot for the age distribution of the selected area
    #Uses function age_distribution from HealthPlots file
    output$age_distr <- renderPlot({
      age_distribution(Gez_datasetInput()$dataset)
    })
    
    #Creates map of the selected subtheme  
    #Uses function make_map_gez from HealthPlots file
    output$map_subtheme <- renderLeaflet({
      subtheme <- selected_subtheme_gez()
      make_map_gez(Gez_datasetInput()$dataset, input$age_map, subtheme, input$categorie_map)
    })
    
    #Selected area name for the box titles (changes only when 'zoeken' button is clicked)
    selected_area_title_gez <- eventReactive(input$action_gez,{
      if(input$niveau_gez=="Gemeenten"){
        title <- input$gemeente1_gez
      }else if(input$niveau_gez=="Wijken"){
        title <- input$wijken2_gez
      }else if(input$niveau_gez=="Buurten"){
        title <- input$buurten3_gez
      }
      return(title)
    })
    
    #Info box, title changes based on the selected area
    output$info_box_gez = renderUI({
      title <- paste0("Informatie over ", selected_area_title_gez())
      box(title = title, width = 3, status = "warning", solidHeader = T,
          "In de tabel hieronder ziet u wat de stedelijkheid en de inkomensgroep zijn voor het gekozen gebied.",
          tableOutput("info_area_gez"),
          "Stedelijkheid: 1 = zeer sterk stedelijk, 5 = niet stedelijk.", br(),
          "Inkomensniveau: 1 = zeer laag percentage, 4 = hoog percentage van huishoudens met een inkomen onder het sociaal minimum.",
         ) # Box informatie
    })
    
    #Age box, title changes based on the selected area
    output$age_box_gez = renderUI({
      title <- paste0("Leeftijdsopbouw ", selected_area_title_gez())
      box(title = title, width = 3, status = "warning", solidHeader = T,
          "In onderstaande staafdiagram  ziet u de leeftijdsopbouw van het gekozen gebied.",
          shinycssloaders::withSpinner(plotOutput("age_distr"))) 
      
    })
    
    #Kaart box, title changes based on the selected area
    output$kaart_box_gez = renderUI({
      title <- paste0("Kaart met ", selected_area_title_gez())
      box(title = title, width = 3, status = "warning", solidHeader = T,
          "Kaart waarop het gekozen gebied te zien is (blauwe pointer) en de gebieden waarmee wordt vergeleken.",
          shinycssloaders::withSpinner(leafletOutput("prime_map2")),
          span(textOutput("error_vergelijkbaarheid_gez"), style="color:red")) 
    })
    
    #Base which plots and where to place them on whether the chosen subtheme is normal or special
    output$plots<-renderUI({
      subtheme <- selected_subtheme_gez()
      if(subtheme %in% Normal){
        column(width =9, 
               fluidRow(
                 box(title = "Kaart", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande kaart zijn de waardes voor het gekozen subthema te zien voor het gekozen gebied (blauwe pointer)
                     en de andere gebeieden waarmee wordt vergeleken.", br(),
                     br(),
                     selectInput("age_map", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     shinycssloaders::withSpinner(leafletOutput('map_subtheme'))
                 ),
                 box(title="Histogram", width=6, status="warning", solidHeader = T,
                     "In onderstaande histogram is de frequentieverdeling voor het gekozen subthema  te zien.
                           De zwarte verticale lijn is de waarde van het gekozen gebied. Hiermee kunt u zien hoe uw gebied het doet ten opzichte van de andere gebieden.", br(),
                     br(),
                     selectInput("norm_age_hist", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     shinycssloaders::withSpinner(plotOutput("gez_hist"))),
                 
               ),
               fluidRow(
                 box(title = "Lijndiagram", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande lijndiagram is de ontwikkeling van het gekozen subthema in de tijd te zien. 
                           De roze lijn is voor het gekozen gebied en de blauwe lijn is het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).",br(),
                     br(),
                     selectInput("norm_age_line", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     shinycssloaders::withSpinner(plotOutput("gez_line_plot"))),
                 box(title = "Staafdiagram per leeftijdsklasse", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande staafdiagram is het percentage voor het gekozen subthema te zien voor de verschillende leeftijdsklasses.
                           In het roze is het gekozen gebied te zien en in het blauw het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).", br(),
                     br(),
                     shinycssloaders::withSpinner(plotOutput("gez_plot"))),
               )
        )
      }else if(subtheme %in% Special){
        column(width =9, 
               fluidRow(
                 box(title = "Staafdiagram per categorie", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande staafdiagram is het percentage voor de verschillende categorieën binnen het subthema te zien.
                      In het roze is het gekozen gebied te zien en in het blauw het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).", br(),
                     br(),
                     selectInput("spec_age_cat", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     shinycssloaders::withSpinner(plotOutput("staaf_cat"))
                 ),
                 box(title = "Kaart", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande kaart zijn de waardes voor het gekozen subthema te zien voor het gekozen gebied (blauwe pointer)
                     en de andere gebeieden waarmee wordt vergeleken.", br(),
                     br(),
                     selectInput("age_map", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     selectInput("categorie_map", "Categorie:", choices = NULL),
                     shinycssloaders::withSpinner(leafletOutput('map_subtheme'))
                 )
               ),
        
              
         fluidRow(
                 box(title="Histogram", width=6, status="warning", solidHeader = T,
                     "In onderstaande histogram is de frequentieverdeling voor de gekozen categorie  te zien. 
                           De zwarte verticale lijn is de waarde van het gekozen gebied. Hiermee kunt u zien hoe uw gebied het doet ten opzichte van de andere gebieden.", br(),
                     br(),
                     selectInput("spec_age_hist", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     selectInput("categorie_hist", "Categorie:", choices = NULL),
                     shinycssloaders::withSpinner(plotOutput("gez_hist"))
                 ),
                 box(title = "Lijndiagram", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande lijndiagram is de ontwikkeling van de gekozen categorie in de tijd te zien. 
                           De roze lijn is voor het gekozen gebied en de blauwe lijn is het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).",br(),
                     br(),
                     selectInput("spec_age_line", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     selectInput("categorie_line", "Categorie:", choices = NULL),
                     shinycssloaders::withSpinner(plotOutput("gez_line_plot"))
                 ),
                 box(title = "Staafdiagram per leeftijdsklasse", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande staafdiagram is het percentage voor de gekozen categorie te zien voor de verschillende leeftijdsklasses.
                           In het roze is het gekozen gebied te zien en in het blauw het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).", br(),
                     br(),
                     selectInput("categorie_staaf", "Categorie:", choices = NULL),
                     shinycssloaders::withSpinner(plotOutput("gez_plot"))
                 ),
               )
        )
      }
    })
})

