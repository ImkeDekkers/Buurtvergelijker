library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)

gemeenten <- readRDS("../Data/gemeenten.rds")
wijken <- readRDS("../Data/wijken.rds")
buurten <- readRDS("../Data/buurten.rds")
postcodes_final <- readRDS("../Data/postcodes_final.rds")

shinyServer(function(input, output, session) {

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
    observeEvent(input$gemeente, {
      updateSelectInput(session, 'wijken',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente]))                                               # Only display that are in the selected gemeente
    })
    observeEvent(input$wijken,{
      updateSelectInput(session, 'buurten',
                        choices = unique(postcodes_final$buurtnaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente & postcodes_final$wijknaam2020==input$wijken])) # Only display buurten that are in the selected wijk
    })
    
    #histogram van de variabelen, op de 3 verschillende niveau's  
    output$histogram <- renderPlot({
        if(input$niveau == "Gemeenten"){
          df_gemeenten <- as.data.frame(gemeenten)                                                                                          # Reshape data to data frame (not with shape files)
          stedelijkheid_num_gem <- df_gemeenten[df_gemeenten$GM_NAAM==input$gemeente, 5]                                                    # Stedelijkheid is the 5th column in the data
          comparable_gemeenten <- gemeenten[gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_gem, ] # Create the right data based on the given stedelijkheid number
          hist_data <- comparable_gemeenten                                                                                                 # Use the subset of the data with the same stedelijkheid number for visualizations
          area_line <- gemeenten %>%
            filter(GM_NAAM == input$gemeente) %>%
            pull(input$variable)
        }else if (input$niveau == "Buurten"){
          df_buurten <- as.data.frame(buurten)
          stedelijkheid_num_buurten <- df_buurten[df_buurten$BU_NAAM==input$buurten, 11]
          comparable_buurten <- buurten[buurten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_buurten, ]  
          hist_data <- comparable_buurten
          area_line <- buurten %>%
            filter(GM_NAAM == input$gemeente & WK_NAAM == input$wijken & BU_NAAM == input$buurten) %>%
            pull(input$variable)
        }else if (input$niveau == "Wijken"){
          df_wijken <- as.data.frame(wijken)
          stedelijkheid_num_wijken <- df_wijken[df_wijken$WK_NAAM==input$wijken, 8]
          comparable_wijken <- wijken[wijken$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_wijken, ]
          hist_data <- comparable_wijken
          area_line <- wijken %>%
            filter(GM_NAAM == input$gemeente & WK_NAAM == input$wijken) %>%
            pull(input$variable)
        }else {
            print("Select een niveau")
        }
        ggplot(hist_data, aes(!!input$variable)) + geom_histogram() + geom_vline(xintercept = area_line)
    })
    
    #kaart met kleur gebaseerd op de gekozen variabelen. Mogelijk op de 3 verschillende niveau's
    output$map <- renderLeaflet({
        #Select de data en label van het gekozen niveau
        if(input$niveau == "Gemeenten"){
          df_gemeenten <- as.data.frame(gemeenten)                                                                                          # Reshape data to data frame (not with shape files)
          stedelijkheid_num_gem <- df_gemeenten[df_gemeenten$GM_NAAM==input$gemeente, 5]                                                    # Stedelijkheid is the 5th column in the data
          comparable_gemeenten <- gemeenten[gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_gem, ] # Create the right data based on the given stedelijkheid number
          map_data <- comparable_gemeenten
          map_data$label <- map_data$GM_NAAM
        } else if (input$niveau == "Buurten"){
          df_buurten <- as.data.frame(buurten)
          stedelijkheid_num_buurten <- df_buurten[df_buurten$WK_NAAM==input$wijken& df_buurten$GM_NAAM==input$gemeente &df_buurten$BU_NAAM==input$buurten, 11]
          comparable_buurten <- buurten[buurten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_buurten, ]
          map_data <- comparable_buurten
          map_data$label <- map_data$BU_NAAM
        }else if (input$niveau == "Wijken"){
          df_wijken <- as.data.frame(wijken)
          stedelijkheid_num_wijken <- df_wijken[df_wijken$WK_NAAM==input$wijken& df_wijken$GM_NAAM==input$gemeente,8]
          comparable_wijken <- wijken[wijken$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_wijken, ]
          map_data <- comparable_wijken
          map_data$label <- map_data$WK_NAAM
        }else {
            print("Selecteer een niveau")
        }
        
        map_data$variableplot <-map_data[[input$variable]]
        # Change the colors based on the selected variable
        pal <- colorBin("YlOrRd", domain = map_data$variableplot)       #maps numeric input data to a fixed number of output colors using binning (slicing the input domain up by value)
        qpal <- colorQuantile("YlOrRd", map_data$variableplot, n = 6)    #maps numeric input data to a fixed number of output colors using quantiles (slicing the input domain into subsets with equal numbers of observations)
        
        coloring <- tryCatch({
          qpal(map_data$variableplot)
        } , error = function(e) {
          pal(map_data$variableplot)
        } )
        
        # Change the labels that appear on hover based on the selected variable
        labels <- sprintf("%s: %g", map_data$label, map_data$variableplot) %>% 
            lapply(htmltools::HTML)
        coloring_legend <- tryCatch({
          leaflet(map_data) %>%
            addPolygons(
              fillColor = ~ coloring,
              color = "black",
              weight = 0.5,
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE),
              label = labels
            ) %>%
            addProviderTiles(providers$CartoDB.Positron) %>% 
            
            leaflet::addLegend(
              pal = qpal, values = ~variableplot, opacity = 0.7, title = NULL, labFormat = function(type, cuts, p) {      #labformat function makes sure the actual values instead of the quantiles are displayed in the legend
                n = length(cuts)
                paste0(cuts[-n], " &ndash; ", cuts[-1])
              }
            )
        }, error = function(e) {
          leaflet(map_data) %>%
            addPolygons(
              fillColor = ~ coloring,
              color = "black",
              weight = 0.5,
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE),
              label = labels
            ) %>%
            addProviderTiles(providers$CartoDB.Positron) %>% 
        
            leaflet::addLegend(
              pal = pal, values = ~variableplot, opacity = 0.7, title = NULL
            )
        })
   
        l <- coloring_legend
    })
    
})

