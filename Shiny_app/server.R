library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)
library(tidyverse)

gemeenten <- readRDS("../Data/gemeenten.rds")
wijken <- readRDS("../Data/wijken.rds")
buurten <- readRDS("../Data/buurten.rds")
postcodes_final <- readRDS("../Data/postcodes_final.rds")

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
      if(input$niveau == "Gemeenten"){
        df_gemeenten <- as.data.frame(gemeenten)   # Reshape data to data frame (not with shape files)
        # Comparable based on stedelijkheid, inkomen and opleiding
        if(input$vergelijkbaar1 == "Stedelijkheidsniveau"){
          stedelijkheid_num_gem <- df_gemeenten[df_gemeenten$GM_NAAM == input$gemeente1, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]      # Stedelijkheid is the 5th column in the data
          comparable_gemeenten <- gemeenten[gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_gem, ]  
        }else if (input$vergelijkbaar1 == "Inkomensniveau"){
          inkomen_num_gem <- df_gemeenten[df_gemeenten$GM_NAAM == input$gemeente1, 'inkomengroep']          # Inkomensniveau (calculated) is 180th column
          comparable_gemeenten <- gemeenten[gemeenten$inkomengroep == inkomen_num_gem, ]
        }else if (input$vergelijkbaar1 == "Opleidingsniveau"){
          opleiding_num_gem <- df_gemeenten[df_gemeenten$GM_NAAM == input$gemeente1, 'opleidingsgroep']        # Opleidingsniveau (calculated) is 182nd column
          comparable_gemeenten <- gemeenten[gemeenten$opleidingsgroep == opleiding_num_gem, ]
        }
        #area_value <- comparable_gemeenten %>% filter(GM_NAAM == input$gemeente1) %>% pull(input$variable)
        #comparable_gemeenten$area_line <- area_value
        dataset <- comparable_gemeenten
        dataset$label <- dataset$GM_NAAM
        selected_polygon <- gemeenten %>% filter(GM_NAAM == input$gemeente1)
        selected_centroid <- st_coordinates(st_centroid(selected_polygon))
        dataset$centroidx <- selected_centroid[1,1]
        dataset$centroidy <- selected_centroid[1,2]
        dataset$code <- dataset$GM_CODE
        dataset$selected_area_code <- dataset %>% filter(GM_NAAM == input$gemeente1) %>% pull(code)
        dataset$selected_area_label <- dataset %>% filter(GM_NAAM == input$gemeente1) %>% pull(label)
      }else if(input$niveau == "Wijken"){
        df_wijken <- as.data.frame(wijken)
        stedelijkheid_num_wk <- df_wijken[df_wijken$WK_NAAM==input$wijken2 & df_wijken$GM_NAAM == input$gemeente2, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
        inkomen_num_wk <- df_wijken[df_wijken$WK_NAAM==input$wijken2 & df_wijken$GM_NAAM == input$gemeente2, "inkomengroep"]
        opleiding_num_wk <- df_wijken[df_wijken$WK_NAAM==input$wijken2 & df_wijken$GM_NAAM == input$gemeente2, "opleidingsgroep"]
        if(input$vergelijkbaar2 == "Stedelijkheidsniveau"){
          comparable_wijken <- wijken[wijken$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_wk, ]
        }
        else if (input$vergelijkbaar2 == "Inkomensniveau"){
          if(is.na(inkomen_num_wk)){
            comparable_wijken <- wijken
            output$ink_vergelijkbaarheid <- renderText(
              print("Let op, door een missende waarde van het inkomensniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
            )
          } #  WANTED TO MAKE SURE THAT IF OPLEIDINGSGROEP IS MISSING, THAT THE WHOLE DATASET IS USED TO GENERATE PLOTS
          else{
            comparable_wijken <- wijken[wijken$inkomengroep == inkomen_num_wk, ]
              
            }  
        } 
        else if (input$vergelijkbaar2 == "Opleidingsniveau"){
          if(is.na(opleiding_num_wk)){
            comparable_wijken <- wijken
            output$opl_vergelijkbaarheid <- renderText(
              print("Let op, door een missende waarde van het opleidingsniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
            )
          }#  WANTED TO MAKE SURE THAT IF OPLEIDINGSGROEP IS MISSING, THAT THE WHOLE DATASET IS USED TO GENERATE PLOTS
          else{
            comparable_wijken <- wijken[wijken$opleidingsgroep == opleiding_num_wk, ]
            
          }
        }
        #area_value <- wijken %>%filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2) %>%pull(input$variable)
        #comparable_wijken$area_line <- area_value
        dataset <- comparable_wijken
        dataset$label <- dataset$WK_NAAM
        selected_polygon <- wijken %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2)
        selected_centroid <- st_coordinates(st_centroid(selected_polygon))
        dataset$centroidx <- selected_centroid[1,1]
        dataset$centroidy <- selected_centroid[1,2]
        dataset$code <- dataset$WK_CODE
        dataset$selected_area_code <- dataset %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2) %>%pull(code)
        dataset$selected_area_label <- dataset %>% filter(GM_NAAM == input$gemeente2 & WK_NAAM == input$wijken2) %>%pull(label)
      }else if (input$niveau == "Buurten"){
        df_buurten <- as.data.frame(buurten)
        stedelijkheid_num_buurten <- df_buurten[df_buurten$BU_NAAM==input$buurten3 & df_buurten$GM_NAAM == input$gemeente3 & df_buurten$WK_NAAM == input$wijken3, 11]
        comparable_buurten <- buurten[buurten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_buurten, ]
        #area_value <- comparable_buurten %>%filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3) %>%pull(input$variable)
        #comparable_buurten$area_line <- area_value
        dataset <- comparable_buurten
        dataset$label <- dataset$BU_NAAM
        selected_polygon <- buurten %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3)
        selected_centroid <- st_coordinates(st_centroid(selected_polygon))
        dataset$centroidx <- selected_centroid[1,1]
        dataset$centroidy <- selected_centroid[1,2]
        dataset$code <- dataset$BU_CODE
        dataset$selected_area_code <- dataset %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3) %>%pull(code)
        dataset$selected_area_label <- dataset %>% filter(GM_NAAM == input$gemeente3 & WK_NAAM == input$wijken3 & BU_NAAM == input$buurten3) %>%pull(label)
      }
      return(dataset)
    })
    
    ###################
    # I THINK THIS IS NOT NEEDED ANYMORE 
    ###################
    
    #histogram van de variabele with vertical line of the value from the gemeente/wijk/buurt that has een selected  
    #output$histogram <- renderPlot({
      #data_hist <- datasetInput()
        #ggplot(data_hist, aes(!!input$variable)) + 
          #geom_histogram(fill='steelblue3', color='#e9ecef', bins=20) + 
          #geom_vline(xintercept = data_hist$`area_line`) +
          #labs( y = "Aantal")
    #})
    
    #map with color based on the chosen variable
    #output$map <- renderLeaflet({
      #map_data  <- datasetInput()  
      #map_data$variableplot <- map_data[[input$variable]]
        # Change the colors based on the selected variable
        #pal <- colorBin("YlOrRd", domain = map_data$variableplot)       #maps numeric input data to a fixed number of output colors using binning (slicing the input domain up by value)
        #qpal <- colorQuantile("YlOrRd", map_data$variableplot, n = 6)    #maps numeric input data to a fixed number of output colors using quantiles (slicing the input domain into subsets with equal numbers of observations)
        
        #for the colors in the map, colorQuantile is used, unless an error is given, then we use colorBin
        #coloring <- tryCatch({
          #qpal(map_data$variableplot)
        #} , error = function(e) {
          #pal(map_data$variableplot)
        #} )
        
        # Change the labels that appear on hover based on the selected variable
        #labels <- sprintf("%s: %g", map_data$label, map_data$variableplot) %>% 
            #lapply(htmltools::HTML)
        #legend_title <- as.character(input$variable)
        
        #Different uses of legend, if there is the error of breaks not unique, then colorbin is used and the legend is established in a different manner
        #coloring_legend <- tryCatch({
          #leaflet(map_data) %>%
            #addPolygons(fillColor = ~ coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
              #highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels) %>%
            #addProviderTiles(providers$CartoDB.Positron) %>% 
            #addCircleMarkers(lng = map_data$centroidx, lat = map_data$centroidy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
            #leaflet::addLegend(pal = qpal, values = ~variableplot, opacity = 0.7, title = legend_title, labFormat = function(type, cuts, p) {      #labformat function makes sure the actual values instead of the quantiles are displayed in the legend
                #n = length(cuts)
                #paste0(cuts[-n], " &ndash; ", cuts[-1])
              #}, labFormat = labelFormat(digits = 0)
            #)
        #}, error = function(e) {
          #leaflet(map_data) %>%
            #addPolygons(fillColor = ~ coloring, color = "black", weight = 0.5, fillOpacity = 0.7,
              #highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE), label = labels) %>%
            #addProviderTiles(providers$CartoDB.Positron) %>% 
            #addCircleMarkers(lng = map_data$centroidx, lat = map_data$centroidy, color = "black", weight = 3, opacity = 0.75, fillOpacity = 0)%>%
            #leaflet::addLegend(pal = pal, values = ~variableplot, opacity = 0.7, title = legend_title)
        #})
   
        #l <- coloring_legend
    #})
    
    ###################
    # I THINK THIS IS NOT NEEDED ANYMORE 
    ###################
    
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
      df_selected <- df %>% subset(code == selected_area_code) %>% select(column1, column2, column3, column4)
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
      plot4("Afstand tot huisartsenpraktijk (km)", "Aantal huisartsenpraktijken binnen 1 km", "Aantal huisartsenpraktijken binnen 3 km", "Aantal huisartsenpraktijken binnen 5 km")
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
    
})

