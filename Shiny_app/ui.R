library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("yeti"),
    
    titlePanel("Histogram en kaart van verschillende variabelen op gemeente- wijk- of buurtniveau"),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput("niveau", "Niveau:", c("Gemeenten" = "Gemeenten",
                                               "Wijken" = "Wijken",
                                               "Buurten" = "Buurten")),
            varSelectInput("variable", "Variable:", Filter(is.numeric, gemeenten))
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Histogram", plotOutput("histogram")), 
                tabPanel("Kaart", leafletOutput("map"))
            )
        )
    )
)