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
      }else if(input$niveau == 'Wijken'){
        if(input$vergelijkbaar2 == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df[df$WK_NAAM == input$wijken2 & df$GM_NAAM == input$gemeente2, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]      # Stedelijkheid is the 5th column in the data
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
        }else if (input$vergelijkbaar2 == "Inkomensniveau"){
          inkomen_num <- df[df$WK_NAAM == input$wijken2 & df$GM_NAAM == input$gemeente2, 'inkomengroep']          # Inkomensniveau (calculated) is 180th column
          if(is.na(inkomen_num)){
            comparable_df <- df[df$Niveau == input$niveau,]
            output$ink_vergelijkbaarheid <- renderText(
              print("Let op, door een missende waarde van het inkomensniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
            )
          }else{
            comparable_df <- df[df$inkomengroep == inkomen_num, ]
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
          }
        } else if(input$vergelijkbaar2 == "Nederland"){
          comparable_df <- df
        }
        comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2) %>%pull(CODE)
        comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2) %>%pull(NAAM)
        selected_polygon <- wijken %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2)
      }else if(input$niveau == 'Buurten'){
        if(input$vergelijkbaar3 == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df[df$BU_NAAM==input$buurten3 & df$GM_NAAM == input$gemeente3 & df$WK_NAAM == input$wijken3, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
        } else if(input$vergelijkbaar3 == "Nederland"){
          comparable_df <- df
        }
        comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3) %>%pull(CODE)
        comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3) %>%pull(NAAM)
        selected_polygon <- buurten %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3)
      } 
      dataset <- st_as_sf(comparable_df)
      selected_centroid <- st_coordinates(st_centroid(selected_polygon))
      dataset$centroidx <- selected_centroid[1,1]
      dataset$centroidy <- selected_centroid[1,2]
      
      return(dataset)
    })
    
    #Function that makes map of the selected variable 
    make_map <- function(variable){
      #function to get values of selected variable 
      map_variable <- function(variable1){
        df <- as.data.frame(datasetInput())
        df_selec_var <- df[[variable1]] 
        return(df_selec_var)
      }
      #get input for the map
      input_map <- map_variable(variable)
      map_data <- datasetInput()
      
      #define colors for polygons and legend 
      pal <- colorBin("YlOrRd", domain = input_map)
      qpal <- colorQuantile("YlOrRd", input_map, n = 6)
      #for the colors in the map, colorQuantile is used, unless an error is given, then we use colorBin
      coloring <- tryCatch({
        qpal(input_map)
      } , error = function(e) {
        pal(input_map)
      } )
      legend_title <- as.character(variable)
      labels <- sprintf("%s: %g", map_data$NAAM, input_map) %>% 
        lapply(htmltools::HTML)
      
      #map
      output_map <- tryCatch({
        leaflet(map_data)%>%
          addPolygons(fillColor = ~coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                      highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels)%>%
          addProviderTiles(providers$CartoDB.Positron)%>%
          addCircleMarkers(lng = map_data$centroidx, lat = map_data$centroidy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
          leaflet::addLegend(pal = qpal, values = ~input_map, opacity = 0.7, title = legend_title, labFormat = function(type, cuts, p) {      #labformat function makes sure the actual values instead of the quantiles are displayed in the legend
            n = length(cuts)
            paste0(cuts[-n], " &ndash; ", cuts[-1])
            }, labFormat = labelFormat(digits = 0))
        
      }, error = function(e) {
        leaflet(map_data) %>%
          addPolygons(fillColor = ~ coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
                    highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels) %>%
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addCircleMarkers(lng = map_data$centroidx, lat = map_data$centroidy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
          leaflet::addLegend(pal = pal, values = ~input_map, opacity = 0.7, title = legend_title)
      })
      
      return(output_map)
      
    }
    
    # Make icon for prime map
    awesome <- makeAwesomeIcon(
      icon = "arrow-down",
      iconColor = "black",
      markerColor = "blue",
      library = "fa")
    
    # Create map to point to the selected location and comparable polygons
    output$prime_map <- renderLeaflet({
      leaflet(datasetInput()) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(color = "navy", weight = 1, 
                    highlightOptions = highlightOptions(color = "black", 
                                                        weight = 2),
                    label = ~htmlEscape(datasetInput()$NAAM)) %>% 
        addAwesomeMarkers(lng = datasetInput()$centroidx,
                          lat = datasetInput()$centroidy,
                          icon = awesome)
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
    
    #Plot_4 of huisartsenpraktijken
    output$plot_huisarts <- renderPlot({
      plot4("Afstand tot huisartsenpraktijk (km)", 
            "Aantal huisartsenpraktijken binnen 1 km", 
            "Aantal huisartsenpraktijken binnen 3 km", 
            "Aantal huisartsenpraktijken binnen 5 km")
    })
    
    #Plot_4 of Ziekenhuis (incl. buitenpolikliniek)
    output$plot_ziekenhuis_incl <- renderPlot({
      plot4("Afstand tot ziekenhuis incl. buitenpolikliniek (km)",                             
            "Aantal ziekenhuizen incl. buitenpolikliniek binnen 5 km" ,                        
            "Aantal ziekenhuizen incl. buitenpolikliniek binnen 10 km" ,                       
            "Aantal ziekenhuizen incl. buitenpolikliniek binnen 20 km")
    })
    
    #Plot_4 of Ziekenhuis (excl. buitenpolikliniek)
    output$plot_ziekenhuis_excl <- renderPlot({
      plot4("Afstand tot ziekenhuis excl. Buitenpolikliniek (km)",                             
            "Aantal ziekenhuizen excl. Buitenpolikliniek binnen 5 km" ,                        
            "Aantal ziekenhuizen excl. Buitenpolikliniek binnen 10 km" ,                       
            "Aantal ziekenhuizen excl. Buitenpolikliniek binnen 20 km")
    })
    
    #Plot_4 of Grote supermarkten
    output$plot_supermarkt <- renderPlot({
      plot4("Afstand tot grote supermarkt (km)",                                               
            "Aantal  grote supermarkten binnen 1 km",                                         
            "Aantal  grote supermarkten binnen 3 km",                                          
            "Aantal  grote supermarkten binnen 5 km")
    })
    
    #Plot_4 of overige dagelijkse levensmiddelen
    output$plot_ov_levensm <- renderPlot({
      plot4("Afstand tot overige dagelijkse levensmiddelen (km)",                              
            "Aantal winkels overige dagelijkse levensmiddelen binnen 1 km",                    
            "Aantal winkels overige dagelijkse levensmiddelen binnen 3 km",                    
            "Aantal winkels overige dagelijkse levensmiddelen binnen 5 km")
    })
    
    #Plot_4 of warenhuis
    output$plot_warenhuis <- renderPlot({
      plot4("Afstand tot warenhuis (km)",                                                      
            "Aantal warenhuizen binnen 5 km",                                                  
            "Aantal warenhuizen binnen 10 km" ,                                                
            "Aantal warenhuizen binnen 20 km")
    })
    
    #Plot_4 of cafes
    output$plot_cafes <- renderPlot({
      plot4("Afstand tot café (km)" ,                                                          
            "Aantal cafés binnen 1 km" ,                                                       
            "Aantal cafés binnen 3 km" ,                                                       
            "Aantal cafés binnen 5 km")
    })
    
    #Plot_4 of cafetaria
    output$plot_cafetaria <- renderPlot({
      plot4("Afstand tot cafetaria (km)",                                                      
            "Aantal cafetaria's binnen 1 km",                                                  
            "Aantal cafetaria's binnen 3 km",                                                  
            "Aantal cafetaria's binnen 5 km")
    })
    
    #Plot_4 of reastaurants
    output$plot_restaurants <- renderPlot({
      plot4("Afstand tot restaurant (km)",                                                     
            "Aantal restaurants binnen 1 km",                                                  
            "Aantal restaurants binnen 3 km",                                                  
            "Aantal restaurants binnen 5 km")
    })
    
    #Plot_4 of hotels
    output$plot_hotels <- renderPlot({
      plot4("Afstand tot hotel (km)",                                                     
            "Aantal hotel binnen 5 km",                                                  
            "Aantal hotel binnen 10 km",                                                  
            "Aantal hotel binnen 20 km")
    })
    
    #Plot_4 of kinderdagverblijf  
    output$plot_kinderdagverblijf   <- renderPlot({
      plot4("Afstand tot kinderdagverblijf  (km)",                                                     
            "Aantal kinderdagverblijf  binnen 1 km",                                                  
            "Aantal kinderdagverblijf  binnen 3 km",                                                  
            "Aantal kinderdagverblijf  binnen 5 km")
    })
    
    #Plot_4 of buitenschoolse opvang  
    output$plot_opvang   <- renderPlot({
      plot4("Afstand tot buitenschoolse opvang  (km)",                                                     
            "Aantal buitenschoolse opvang  binnen 1 km",                                                  
            "Aantal buitenschoolse opvang  binnen 3 km",                                                  
            "Aantal buitenschoolse opvang  binnen 5 km")
    })
    
    #Plot_4 of basisscholen  
    output$plot_basisscholen   <- renderPlot({
      plot4("Afstand tot basisscholen (km)",                                                     
            "Aantal basisscholen binnen 1 km",                                                  
            "Aantal basisscholen binnen 3 km",                                                  
            "Aantal basisscholen binnen 5 km")
    })
    
    #Plot_4 of voortgezet onderwijs  
    output$plot_vo   <- renderPlot({
      plot4("Afstand tot voortgezet onderwijs (km)",                                                     
            "Aantal voortgezet onderwijs binnen 3 km",                                                  
            "Aantal voortgezet onderwijs binnen 5 km",                                                  
            "Aantal voortgezet onderwijs binnen 10 km")
    })
    
    #Plot_4 of scholen VMBO  
    output$plot_VMBO   <- renderPlot({
      plot4("Afstand tot scholen VMBO (km)",                                                     
            "Aantal scholen VMBO binnen 3 km",                                                  
            "Aantal scholen VMBO binnen 5 km",                                                  
            "Aantal scholen VMBO binnen 10 km")
    })
    
    #Plot_4 of scholen HAVO/VWO  
    output$plot_HAVO_VWO   <- renderPlot({
      plot4("Afstand tot scholen HAVO/VWO (km)",                                                     
            "Aantal scholen HAVO/VWO binnen 3 km",                                                  
            "Aantal scholen HAVO/VWO binnen 5 km",                                                  
            "Aantal scholen HAVO/VWO binnen 10 km")
    })
    
    #Plot_4 of bioscoop   
    output$plot_bioscoop   <- renderPlot({
      plot4("Afstand tot bioscoop (km)",                                                     
            "Aantal bioscoop binnen 5 km",                                                  
            "Aantal bioscoop binnen 10 km",                                                  
            "Aantal bioscoop binnen 20 km")
    })
    
    #Plot_4 of attracties   
    output$plot_attractie  <- renderPlot({
      plot4("Afstand tot attractie (km)",                                                     
            "Aantal attracties binnen 10 km",                                                  
            "Aantal attracties binnen 20 km",                                                  
            "Aantal attracties binnen 50 km")
    })
    
    #Plot_4 of podiumkunsten   
    output$plot_podiumkunsten  <- renderPlot({
      plot4("Afstand tot podiumkunsten (km)",                                                     
            "Aantal podiumkunsten binnen 5 km",                                                  
            "Aantal podiumkunsten binnen 10 km",                                                  
            "Aantal podiumkunsten binnen 20 km")
    })
    
    #Plot_4 of musea   
    output$plot_musea  <- renderPlot({
      plot4("Afstand tot museum (km)",                                                     
            "Aantal musea binnen 5 km",                                                  
            "Aantal musea binnen 10 km",                                                  
            "Aantal musea binnen 20 km")
    })
    
    #map of huisartsen 
    output$map_huisarts <- renderLeaflet({
      make_map("Afstand tot huisartsenpraktijk (km)")
    })
    
    #map of ziekenhuizen incl
    output$map_ziekenhuizen_incl <- renderLeaflet({
      make_map("Afstand tot ziekenhuis incl. buitenpolikliniek (km)")
    })
    
    #map of ziekenhuizen excl
    output$map_ziekenhuizen_excl <- renderLeaflet({
      make_map("Afstand tot ziekenhuis excl. Buitenpolikliniek (km)")
    })
   
    #map of supermarkt 
    output$map_supermarkt <- renderLeaflet({
      make_map("Afstand tot grote supermarkt (km)")
    })
    
    #map of overige dagelijkse levensmiddelen 
    output$map_ov_levensm <- renderLeaflet({
      make_map("Afstand tot overige dagelijkse levensmiddelen (km)")
    })
    
    #map of warenhuis
    output$map_warenhuis <- renderLeaflet({
      make_map("Afstand tot warenhuis (km)")
    })
    
    #map of cafes
    output$map_cafes <- renderLeaflet({
      make_map("Afstand tot café (km)")
    })
    
    #map of cafetarias 
    output$map_cafetaria <- renderLeaflet({
      make_map("Afstand tot cafetaria (km)")
    })
    
    #map of restaurant
    output$map_restaurants <- renderLeaflet({
      make_map("Afstand tot restaurant (km)")
    })
    
    #map of hotel
    output$map_hotels <- renderLeaflet({
      make_map("Afstand tot hotel (km)")
    })
    
    #map of kinderdagverblijf
    output$map_kinderdagverblijf <- renderLeaflet({
      make_map("Afstand tot kinderdagverblijf  (km)")
    })
    
    #map of buitenschoolse opvang 
    output$map_opvang <- renderLeaflet({
      make_map("Afstand tot buitenschoolse opvang  (km)")
    })
    
    #map of basisscholen
    output$map_basisscholen <- renderLeaflet({
      make_map("Afstand tot basisscholen (km)")
    })
    
    #map of voorgezetonderwijs  
    output$map_vo <- renderLeaflet({
      make_map("Afstand tot voortgezet onderwijs (km)")
    })
    
    #map of VMBO
    output$map_VMBO <- renderLeaflet({
      make_map("Afstand tot scholen VMBO (km)")
    })
    
    #map of HAVO/VWO 
    output$map_HAVO_VWO <- renderLeaflet({
      make_map("Afstand tot scholen HAVO/VWO (km)")
    })
    
    #map of bioscoop
    output$map_bioscoop <- renderLeaflet({
      make_map("Afstand tot bioscoop (km)")
    })
    
    #map of attractie 
    output$map_attractie <- renderLeaflet({
      make_map("Afstand tot attractie (km)")
    })
    
    #map of podiumkunsten  
    output$map_podiumkunsten <- renderLeaflet({
      make_map("Afstand tot podiumkunsten (km)")
    })
    
    #map of musea
    output$map_musea <- renderLeaflet({
      make_map("Afstand tot museum (km)")
    })
})

