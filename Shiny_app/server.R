library(shiny)
library(ggplot2)
library(sf)
library(leaflet)


shinyServer(function(input, output) {
    
    #Finding gemeente, wijk and buurt based on the input postcode
    output$postcode_info <- renderText(
      if(any(postcodes_final$PC6==input$postcode)){
        matching_postcode <- postcodes_final %>% filter_at(vars(PC6), any_vars(. %in% input$postcode))     
        with(matching_postcode, sprintf('Uw gemeentenaam is %s, uw wijknaam is %s en uw buurtnaam is %s', Gemeentenaam2020, wijknaam2020, buurtnaam2020))
    } else {
        print("Uw postcode wordt niet herkend, probeer een andere postcode")
      }
    )
  
    #histogram van de variabelen, op de 3 verschillende niveau's  
    output$histogram <- renderPlot({
        if(input$niveau == "Gemeenten"){
           hist_data <- gemeenten
        } else if (input$niveau == "Buurten"){
            hist_data <- buurten
        }else if (input$niveau == "Wijken"){
            hist_data <- wijken
        }else {
            print("Select een niveau")
        }
        ggplot(hist_data, aes(!!input$variable)) + geom_histogram()
    })
    
    output$histogram_expl <- renderText("Wanneer u een niveau en variabele heeft geselecteerd, kunt u in deze grafiek zien hoe de verdeling van deze variabele is op het geselecteerde niveau")

    #kaart met kleur gebaseerd op de gekozen variabelen. Mogelijk op de 3 verschillende niveau's
    output$map <- renderLeaflet({
        #Select de data en label van het gekozen niveau
        if(input$niveau == "Gemeenten"){
            map_data <- gemeenten
            map_data$label <- map_data$GM_NAAM
        } else if (input$niveau == "Buurten"){
            map_data <- buurten
            map_data$label <- map_data$BU_NAAM
        }else if (input$niveau == "Wijken"){
            map_data <- wijken
            map_data$label <- map_data$WK_NAAM
        }else {
            print("Select een niveau")
        }
        
        map_data$variableplot <-map_data[[input$variable]]
        # Change the colors based on the selected variable
        #pal <- colorBin("YlOrRd", domain = map_data$variableplot)       #maps numeric input data to a fixed number of output colors using binning (slicing the input domain up by value)
        qpal <- colorQuantile("YlOrRd", map_data$variableplot, n = 6)   #maps numeric input data to a fixed number of output colors using quantiles (slicing the input domain into subsets with equal numbers of observations)
        
        
        # Change the labels that appear on hover based on the selected variable
        labels <- sprintf("%s: %g", map_data$label, map_data$variableplot) %>% 
            lapply(htmltools::HTML)
        
        # Change the map based on selected variable
        l <- leaflet(map_data) %>%
            addPolygons(
                fillColor = ~ qpal(variableplot),
                color = "black",
                weight = 0.5,
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE),
                label = labels
            ) %>%
          
            leaflet::addLegend(
                pal = qpal, values = ~variableplot, opacity = 0.7, title = NULL, labFormat = function(type, cuts, p) {      #labformat function makes sure the actual values instead of the quantiles are displayed in the legend
                  n = length(cuts)
                  paste0(cuts[-n], " &ndash; ", cuts[-1])
                }
            )
    })
    output$map_expl <- renderText("Wanneer u een niveau en variabele heeft geselecteerd, kunt u op deze kaart zien welke waarde van deze variabele hoort bij elke gemeente/wijk/buurt")

})
