library(shiny)
library(ggplot2)
library(sf)
library(leaflet)


shinyServer(function(input, output) {
  
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
        pal <- colorBin("YlOrRd", domain = map_data$variableplot)
        
        # Change the labels that appear on hover based on the selected variable
        labels <- sprintf("%s: %g", map_data$label, map_data$variableplot) %>% 
            lapply(htmltools::HTML)
        
        # Change the map based on selected variable
        l <- leaflet(map_data) %>%
            addPolygons(
                fillColor = ~ pal(variableplot),
                color = "black",
                weight = 0.5,
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(color='white',weight=0.5,fillOpacity = 0.7, bringToFront = TRUE),
                label = labels
            ) %>%
            leaflet::addLegend(
                pal = pal, values = ~variableplot,
                opacity = 0.7, title = NULL
            )
    })

})
