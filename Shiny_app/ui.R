library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("yeti"),
    
    titlePanel("Histogram en kaart van verschillende variabelen op gemeente- wijk- of buurtniveau"),
    
    sidebarLayout(
        
        sidebarPanel(
            textInput("postcode", "Weet u niet uw exacte gemeente, wijk of buurt naam? Vul dan hier uw 6-cijferige postcode in")
        ),
        
        mainPanel(
            tabPanel("Postcode informatie", textOutput("postcode_info"))
        )
    ),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput("niveau", "Niveau:", c("Gemeenten" = "Gemeenten",
                                               "Wijken" = "Wijken",
                                               "Buurten" = "Buurten")),
            varSelectInput("variable", "Variabele:", Filter(is.numeric, gemeenten))
        ),
       
        mainPanel(
            tabsetPanel(
                tabPanel("Histogram", plotOutput("histogram"), textOutput("histogram_expl")), 
                tabPanel("Kaart", leafletOutput("map"), textOutput("map_expl"))
            )
        )
    )
    
    
)