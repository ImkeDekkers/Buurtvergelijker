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

shinyServer(function(input, output, session) {
    
    #remove spaces and change lower case to upper case in postcode
    postcode1 <-  reactive({str_replace_all(input$postcode, fixed(" "), "")})
    postcode <- reactive({toupper(postcode1())})
      
    #Finding gemeente, wijk and buurt based on the input postcode
    output$postcode_info <- renderText(
      if(any(postcodes_final$PC6==postcode())){
        matching_postcode <- postcodes_final %>% filter_at(vars(PC6), any_vars(. %in% postcode()))     
        with(matching_postcode, sprintf('Uw gemeentenaam is %s, uw wijknaam is %s en uw buurtnaam is %s', Gemeentenaam2020, wijknaam2020, buurtnaam2020))
    } else {
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
                          choices = c("Huisartsenpraktijk", "Ziekenhuis"))
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
                          choices = c("Basisschool", "Voorgezet onderwijs", "VMBO school", "HAVO/VWO school"))
      }else if (input$thema == "Verkeer en vervoer") {
        updateSelectInput(session, 'subthema', 
                          choices = c("")) 
        }else if (input$thema == "Vrije tijd en cultuur") {
                            updateSelectInput(session, 'subthema', 
                                              choices = c("Bioscoop", "Attractie", "Podiumkunsten", "Museum"))
        }
          })
    
    #make used data reactive on the selected niveau
    datasetInput <- reactive({
      df <- as.data.frame(full_data)
      df <- df[df$Niveau == input$niveau,]
      if(input$niveau == 'Gemeenten'){
        if(input$vergelijkbaar1 == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df[df$GM_NAAM == input$gemeente1, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]      # Stedelijkheid is the 5th column in the data
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
        }else if (input$vergelijkbaar1 == "Inkomensniveau"){
          inkomen_num <- df[df$GM_NAAM == input$gemeente1, 'inkomengroep']          # Inkomensniveau (calculated) is 180th column
          comparable_df <- df[df$inkomengroep == inkomen_num, ]
        }else if (input$vergelijkbaar1 == "Opleidingsniveau"){
          opleiding_num <- df[df$GM_NAAM == input$gemeente1, 'opleidingsgroep']        # Opleidingsniveau (calculated) is 182nd column
          comparable_df <- df[df$opleidingsgroep == opleiding_num, ]
        } else if(input$vergelijkbaar1 == "Nederland"){
          comparable_df <- df
        }
        comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente1) %>% pull(CODE)
        comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente1) %>% pull(NAAM)
        selected_polygon <- gemeenten %>% filter(GM_NAAM == input$gemeente1)
        row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente1)
      }else if(input$niveau == 'Wijken'){
        if(input$vergelijkbaar2 == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df[df$WK_NAAM == input$wijken2 & df$GM_NAAM == input$gemeente2, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]      # Stedelijkheid is the 5th column in the data
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
          comparable_df <- comparable_df %>% drop_na(CODE)
        }else if (input$vergelijkbaar2 == "Inkomensniveau"){
          inkomen_num <- df[df$WK_NAAM == input$wijken2 & df$GM_NAAM == input$gemeente2, 'inkomengroep']          # Inkomensniveau (calculated) is 180th column
          if(is.na(inkomen_num)){
            comparable_df <- df[df$Niveau == input$niveau,]
            output$ink_vergelijkbaarheid <- renderText(
              print("Let op, door een missende waarde van het inkomensniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
            )
          }else{
            comparable_df <- df[df$inkomengroep == inkomen_num, ]
            comparable_df <- comparable_df %>% drop_na(CODE)
          }
        }else if (input$vergelijkbaar2 == "Opleidingsniveau"){
          opleiding_num <- df[df$WK_NAAM == input$wijken2 & df$GM_NAAM == input$gemeente2, 'opleidingsgroep']        # Opleidingsniveau (calculated) is 182nd column
          if(is.na(opleiding_num)){
            comparable_df <- df[df$Niveau == input$niveau,]
            output$opl_vergelijkbaarheid <- renderText(
              print("Let op, door een missende waarde van het opleidingsniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
            )
          }else{
            comparable_df <- df[df$opleidingsgroep == opleiding_num, ]
            comparable_df <- comparable_df %>% drop_na(CODE)
          }
        } else if(input$vergelijkbaar2 == "Nederland"){
          comparable_df <- df
        }
        comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2) %>%pull(CODE)
        comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2) %>%pull(NAAM)
        selected_polygon <- wijken %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2)
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
        selected_polygon <- buurten %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3)
        row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente3 & comparable_df$WK_NAAM == input$wijken3 & comparable_df$BU_NAAM == input$buurten3)
      } 
      comparable_df$centroidxx <- comparable_df[row_num_selected, 'centroidx']
      comparable_df$centroidyy <- comparable_df[row_num_selected, 'centroidy']
      dataset <- st_as_sf(comparable_df)
      
      return(dataset)
    })
    
    #Function that returns the 5 most similar areas to the input area based on all voorzieningen variables
    top5_distances_overall <- function(){
      
      df <- as.data.frame(datasetInput())
      
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
      
      #Returning GM_NAAM, WK_NAAM and BU_NAAM based on selected niveau
      # if(input$niveau=="Gemeenten"){
      #   final <- final %>% select(GM_NAAM)
      #   final <- rename(final, "Gemeente naam"=GM_NAAM)
      #   row.names(final) <- NULL
      # }else if (input$niveau=="Wijken"){
      #   final <- final %>% unite(., col = "Wijk naam",  WK_NAAM, GM_NAAM, na.rm=TRUE, sep = " (gemeente ")
      #   final$`Wijk naam` <- paste0(final$`Wijk naam`, ")")
      #   final <- final %>% select(`Wijk naam`)
      #   row.names(final) <- NULL
      # }else if (input$niveau=="Buurten"){
      #   final <- final %>% unite(., col = "Buurt naam",  BU_NAAM, GM_NAAM, na.rm=TRUE, sep = " (gemeente ")
      #   final <- final %>% unite(., col = "Buurt naam",  `Buurt naam`, WK_NAAM, na.rm=TRUE, sep = ", wijk ")
      #   final$`Buurt naam` <- paste0(final$`Buurt naam`, ")")
      #   final <- final %>% select(`Buurt naam`)
      #   row.names(final) <- NULL
      # }
      # 
      return(final)
    }
    
    #Gives table output with 5 most similar areas based on all voorzieningen variables
    output$top5_algemeen <- renderTable(
      top5_distances_overall(),
      rownames = TRUE
    )
    
    #Function that returns the 5 most similar areas to the input area based on voorzieningen variables in chosen theme
    top5_distances_theme <- function(){
      
      df <- as.data.frame(datasetInput())
      
      #Making CODE the row index so all rows are identifiable 
      result <-  subset(df, select = -c(CODE))
      row.names(result) <- df$CODE
      
      #subset df based on theme input so it contains the right voorzieningen variables and transform the data with z-score (scale function)
      if(input$thema=="Gezondheid en welzijn"){
        result <- subset(result, select= `Afstand tot huisartsenpraktijk (km)`: `Aantal ziekenhuizen excl. Buitenpolikliniek binnen 20 km`)
      }else if(input$thema=="Detailhandel"){
        result <- subset(result, select= `Afstand tot grote supermarkt (km)`: `Aantal warenhuizen binnen 20 km`)
      }else if(input$thema=="Horeca"){
        result <- subset(result, select= `Afstand tot café (km)`: `Aantal hotel binnen 20 km`)
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
      
      #Returning GM_NAAM, WK_NAAM and BU_NAAM based on selected niveau
      # if(input$niveau=="Gemeenten"){
      #   final <- final %>% select(GM_NAAM)
      #   final <- rename(final, "Gemeente naam"=GM_NAAM)
      #   row.names(final) <- NULL
      # }else if (input$niveau=="Wijken"){
      #   final <- final %>% unite(., col = "Wijk naam",  WK_NAAM, GM_NAAM, na.rm=TRUE, sep = " (gemeente ")
      #   final$`Wijk naam` <- paste0(final$`Wijk naam`, ")")
      #   final <- final %>% select(`Wijk naam`)
      #   row.names(final) <- NULL
      # }else if (input$niveau=="Buurten"){
      #   final <- final %>% unite(., col = "Buurt naam",  BU_NAAM, GM_NAAM, na.rm=TRUE, sep = " (gemeente ")
      #   final <- final %>% unite(., col = "Buurt naam",  `Buurt naam`, WK_NAAM, na.rm=TRUE, sep = ", wijk ")
      #   final$`Buurt naam` <- paste0(final$`Buurt naam`, ")")
      #   final <- final %>% select(`Buurt naam`)
      #   row.names(final) <- NULL
      # }
      
      return(final)
    }
    
    # Make icon for maps
    awesome1 <- makeAwesomeIcon(
      icon = "arrow-down",
      iconColor = "black",
      markerColor = "blue",
      library = "fa")
    
    awesome5 <- makeAwesomeIcon(
      icon = "arrow-down",
      iconColor = "black",
      markerColor = "red",
      library = "fa")
    
    #Function that makes map of the selected variable 
    make_map <- function(variable){
      #get input for the map
      map_data <- datasetInput()
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
      #label_content <- sprintf("%s: %g \n %s: %g", map_data$selected_area_label, map_data$variable[map_data$NAAM == map_data$selected_area_label], "Gemiddelde geselecteerde gebieden", round(mean(map_data$variable, na.rm=TRUE), digits = 1))
      label_content <- sprintf("%s: %g %s: %g", map_data$selected_area_label, map_data$variable[map_data$NAAM == map_data$selected_area_label], "Gemiddelde geselecteerde gebieden", round(mean(map_data$variable, na.rm=TRUE), digits = 1))
      
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
                            icon = awesome5,
                            label = ~NAAM) %>% 
          #addCircleMarkers(lng = map_data$centroidxx, lat = map_data$centroidyy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
          leaflet::addLegend(pal = qpal, values = ~map_data$variable, opacity = 0.7, title = legend_title, labFormat = function(type, cuts, p) {      #labformat function makes sure the actual values instead of the quantiles are displayed in the legend
            n = length(cuts)
            paste0(cuts[-n], " &ndash; ", cuts[-1])
          }, labFormat = labelFormat(digits = 0))
        
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
                            icon = awesome5,
                            label = ~NAAM) %>% 
          #addCircleMarkers(lng = map_data$centroidx, lat = map_data$centroidy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
          leaflet::addLegend(pal = pal, values = ~map_data$variable, opacity = 0.7, title = legend_title)
      })
      
      return(output_map)
      
    }

    #returns table with top 5 similar areas based on chosen theme
    output$top5_theme <- renderTable(
      top5_distances_theme(),
      rownames = TRUE
    )
    
    # Create map to point to the selected location and comparable polygons
    output$prime_map <- renderLeaflet({
      leaflet(datasetInput()) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(color = "navy", weight = 1, 
                    highlightOptions = highlightOptions(color = "black", 
                                                        weight = 2),
                    label = ~htmlEscape(datasetInput()$NAAM)) %>% 
        addAwesomeMarkers(lng = datasetInput()$centroidxx,
                          lat = datasetInput()$centroidyy,
                          icon = awesome1) %>% 
        addAwesomeMarkers(data = top5_distances_overall(),
                          lng = ~centroidx,
                          lat = ~centroidy,
                          icon = awesome5,
                          label = ~NAAM)
    })
    
    #Function that takes four column names and creates a barplot of the selected area and the mean of comparable areas
    plot4 <- function(column1, column2, column3, column4){
      
      #Calculating the mean values of the input columns
      df <- as.data.frame(datasetInput())
      df_gem <- select(df, column1, column2, column3, column4)
      df_gem <- as.data.frame(colMeans(df_gem, na.rm = TRUE))
      df_gem <-  rownames_to_column(df_gem)
      df_gem$groep <- "Gemiddelde vergelijkbare gebieden"
      df_gem <- rename(df_gem, c(Variabele = 1, Waarde = 2, groep=3))
      
      #Looking for the values of the input columns from the selected areas
      selected_area_code <- df[1, "selected_area_code"]
      selected_area_label <- df[1, "selected_area_label"]
      df_selected <- df %>% subset(CODE == selected_area_code) %>% select(column1, column2, column3, column4)
      df_selected <- rownames_to_column(as.data.frame(t(df_selected)))
      df_selected$groep <- selected_area_label
      df_selected <- rename(df_selected, c(Variabele = 1, Waarde = 2, groep = 3))
      
      #adding mean and selected together
      df_final <- rbind(df_gem, df_selected)
      
      #Plot
      ggplot(df_final, aes(x = Variabele, y = Waarde, fill = groep)) + geom_col(position = "dodge") + 
        scale_x_discrete(labels = function(x) 
          stringr::str_wrap(x, width = 15))
    }
    
    output$map_variable <- renderLeaflet({
      if (input$subthema == "Huisartsenpraktijk"){
        make_map("Afstand tot huisartsenpraktijk (km)")
      }else if (input$subthema == "Ziekenhuis"){
        make_map("Afstand tot ziekenhuis incl. buitenpolikliniek (km)")
      }else if (input$subthema == "Supermarkt"){
        make_map("Afstand tot grote supermarkt (km)")
      }else if (input$subthema == "Overige dagelijkse levensmiddelen"){
        make_map("Afstand tot overige dagelijkse levensmiddelen (km)")
      }else if (input$subthema == "Warenhuis"){
        make_map("Afstand tot warenhuis (km)")
      }else if (input$subthema == "Café"){
        make_map("Afstand tot café (km)")
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
      }else if (input$subthema == "Bioscoop"){
        make_map("Afstand tot bioscoop (km)")
      }else if (input$subthema == "Attractie"){
        make_map("Afstand tot attractie (km)")
      }else if (input$subthema == "Podiumkunsten"){
        make_map("Afstand tot podiumkunsten (km)")
      }else if (input$subthema == "Museum"){
        make_map("Afstand tot museum (km)")
      }
    })
    
    output$plot_variable <- renderPlot({
      if (input$subthema == "Huisartsenpraktijk"){
      plot4("Afstand tot huisartsenpraktijk (km)", 
            "Aantal huisartsenpraktijken binnen 1 km", 
            "Aantal huisartsenpraktijken binnen 3 km", 
            "Aantal huisartsenpraktijken binnen 5 km")
      }else if (input$subthema == "Ziekenhuis"){
        plot4("Afstand tot ziekenhuis incl. buitenpolikliniek (km)",                             
              "Aantal ziekenhuizen incl. buitenpolikliniek binnen 5 km",                        
              "Aantal ziekenhuizen incl. buitenpolikliniek binnen 10 km",                       
              "Aantal ziekenhuizen incl. buitenpolikliniek binnen 20 km")
      }else if (input$subthema == "Supermarkt"){
        plot4("Afstand tot grote supermarkt (km)",                                               
              "Aantal  grote supermarkten binnen 1 km",                                         
              "Aantal  grote supermarkten binnen 3 km",                                          
              "Aantal  grote supermarkten binnen 5 km")
      }else if (input$subthema == "Overige dagelijkse levensmiddelen"){
        plot4("Afstand tot overige dagelijkse levensmiddelen (km)",                              
              "Aantal winkels overige dagelijkse levensmiddelen binnen 1 km",                    
              "Aantal winkels overige dagelijkse levensmiddelen binnen 3 km",                    
              "Aantal winkels overige dagelijkse levensmiddelen binnen 5 km")
      }else if (input$subthema == "Warenhuis"){
        plot4("Afstand tot warenhuis (km)",                                                      
              "Aantal warenhuizen binnen 5 km",                                                  
              "Aantal warenhuizen binnen 10 km",                                                
              "Aantal warenhuizen binnen 20 km")
      }else if (input$subthema == "Café"){
        plot4("Afstand tot café (km)" ,                                                          
              "Aantal cafés binnen 1 km" ,                                                       
              "Aantal cafés binnen 3 km" ,                                                       
              "Aantal cafés binnen 5 km")
      }else if (input$subthema == "Cafetaria"){
        plot4("Afstand tot cafetaria (km)",                                                      
              "Aantal cafetaria's binnen 1 km",                                                  
              "Aantal cafetaria's binnen 3 km",                                                  
              "Aantal cafetaria's binnen 5 km")
      }else if (input$subthema == "Restaurant"){
        plot4("Afstand tot restaurant (km)",                                                     
              "Aantal restaurants binnen 1 km",                                                  
              "Aantal restaurants binnen 3 km",                                                  
              "Aantal restaurants binnen 5 km")
      }else if (input$subthema == "Hotel"){
        plot4("Afstand tot hotel (km)",                                                     
              "Aantal hotel binnen 5 km",                                                  
              "Aantal hotel binnen 10 km",                                                  
              "Aantal hotel binnen 20 km")
      }else if (input$subthema == "Kinderdagverblijf"){
        plot4("Afstand tot kinderdagverblijf  (km)",                                                     
              "Aantal kinderdagverblijf  binnen 1 km",                                                  
              "Aantal kinderdagverblijf  binnen 3 km",                                                  
              "Aantal kinderdagverblijf  binnen 5 km")
      }else if (input$subthema == "Buitenschoolse opvang"){
        plot4("Afstand tot buitenschoolse opvang  (km)",                                                     
              "Aantal buitenschoolse opvang  binnen 1 km",                                                  
              "Aantal buitenschoolse opvang  binnen 3 km",                                                  
              "Aantal buitenschoolse opvang  binnen 5 km")
      }else if (input$subthema == "Basisschool"){
        plot4("Afstand tot basisscholen (km)",                                                     
              "Aantal basisscholen binnen 1 km",                                                  
              "Aantal basisscholen binnen 3 km",                                                  
              "Aantal basisscholen binnen 5 km")
      }else if (input$subthema == "Voortgezet onderwijs"){
        plot4("Afstand tot voortgezet onderwijs (km)",                                                     
              "Aantal voortgezet onderwijs binnen 3 km",                                                  
              "Aantal voortgezet onderwijs binnen 5 km",                                                  
              "Aantal voortgezet onderwijs binnen 10 km")
      }else if (input$subthema == "VMBO school"){
        plot4("Afstand tot scholen VMBO (km)",                                                     
              "Aantal scholen VMBO binnen 3 km",                                                  
              "Aantal scholen VMBO binnen 5 km",                                                  
              "Aantal scholen VMBO binnen 10 km")
      }else if (input$subthema == "HAVO/VWO school"){
        plot4("Afstand tot scholen HAVO/VWO (km)",                                                     
              "Aantal scholen HAVO/VWO binnen 3 km",                                                  
              "Aantal scholen HAVO/VWO binnen 5 km",                                                  
              "Aantal scholen HAVO/VWO binnen 10 km")
      }else if (input$subthema == "Bioscoop"){
        plot4("Afstand tot bioscoop (km)",                                                     
              "Aantal bioscoop binnen 5 km",                                                  
              "Aantal bioscoop binnen 10 km",                                                  
              "Aantal bioscoop binnen 20 km")
      }else if (input$subthema == "Attractie"){
        plot4("Afstand tot attractie (km)",                                                     
              "Aantal attracties binnen 10 km",                                                  
              "Aantal attracties binnen 20 km",                                                  
              "Aantal attracties binnen 50 km")
      }else if (input$subthema == "Podiumkunsten"){
        plot4("Afstand tot podiumkunsten (km)",                                                     
              "Aantal podiumkunsten binnen 5 km",                                                  
              "Aantal podiumkunsten binnen 10 km",                                                  
              "Aantal podiumkunsten binnen 20 km")
      }else if (input$subthema == "Museum"){
          plot4("Afstand tot museum (km)",                                                     
                "Aantal musea binnen 5 km",                                                  
                "Aantal musea binnen 10 km",                                                  
                "Aantal musea binnen 20 km")
      }
      })
    
})

