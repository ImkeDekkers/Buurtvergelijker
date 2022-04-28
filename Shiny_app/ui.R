library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(shinythemes)
library(shinydashboard)

#tags$head(tags$style(HTML('.box{box-shadow: none;border-style: none;}'))),

ui <- dashboardPage(
    dashboardHeader(title = "Buurtvergelijker"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Tab2", tabName = "Tab2", icon = icon("th"))
        )
    ),
    
<<<<<<< HEAD
    sidebarLayout(
        
        sidebarPanel(
            textInput("gemeentenaam", "Vul hier uw gemeente-, wijk- of buurtnaam in uit de vorige stap"),
            submitButton("Zoek")
        ),
        
        mainPanel(
            tabPanel("Postcode informatie", textOutput("gemeentenaam"))
        )
    ),
 
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput("niveau", "Niveau:", c("Gemeenten" = "Gemeenten",
                                               "Wijken" = "Wijken",
                                               "Buurten" = "Buurten")),
            varSelectInput("variable", "Variabele:", Filter(is.numeric, gemeenten)),
            submitButton("Indienen")
        ),
       
        mainPanel(
            tabsetPanel(
                tabPanel("Histogram", plotOutput("histogram"), textOutput("histogram_expl")), 
                tabPanel("Kaart", leafletOutput("map"), textOutput("map_expl"))
=======
    dashboardBody(tags$head(tags$style(HTML('.box{box-shadow: none;border-style: none;}'))),
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        h2("Histogram en kaart van verschillende variabelen op gemeente-, wijk- of buurtniveau"),
                        box(width = 3,
                            textInput("postcode", "Weet u niet uw exacte gemeente, wijk of buurt naam? Vul dan hier uw 6-cijferige postcode in"),
                            submitButton("Zoek"),
                            br(),
                            textOutput("postcode_info"))
                    ),
                    
                    fluidRow(
                        box(width = 3,
                            selectInput("niveau", "Niveau:", c("Gemeenten" = "Gemeenten",
                                                               "Wijken" = "Wijken",
                                                               "Buurten" = "Buurten")),
                            selectInput("gemeente", "Gemeente:", choices=c(gemeenten$GM_NAAM)),
                            varSelectInput("variable", "Variabele:", Filter(is.numeric, gemeenten)),
                            submitButton("Indienen"))
                    ),
                    fluidRow(
                        box(title = "Histogram", 
                            "Wanneer u een niveau en variabele heeft geselecteerd, kunt u in deze grafiek zien hoe de verdeling van deze variabele is op het geselecteerde niveau", br(), 
                            br(),
                            plotOutput("histogram")),
                        box(title = "Kaart", 
                            "Wanneer u een niveau en variabele heeft geselecteerd, kunt u op deze kaart zien welke waarde van deze variabele hoort bij elke gemeente/wijk/buurt", br(), 
                            br(),
                            leafletOutput("map"))
                    )
                ),
            
            # Second tab content
            tabItem(tabName = "Tab2",
                    h2("Voor eventueel ander tab ")
>>>>>>> 13a796e43efc318abe72be07ed9d5b657a256800
            )
        )
    )
)