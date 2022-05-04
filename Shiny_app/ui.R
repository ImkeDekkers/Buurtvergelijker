library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(shinythemes)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Buurtvergelijker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Tab2", tabName = "Tab2", icon = icon("th"))
    )
  ),
  
  dashboardBody(tags$head(tags$style(HTML('.box{box-shadow: none;border-style: none;}'))),
                tabItems(
                  # First tab content
                  tabItem(tabName = "dashboard",
                          h2("Histogram en kaart van verschillende variabelen op gemeente-, wijk- of buurtniveau"),
                          fluidRow(
                            box(width = 3,
                                textInput("postcode", "Weet u niet uw exacte gemeente, wijk of buurt naam? Vul dan hier uw postcode in:"),
                                textOutput("postcode_info"))
                          ),
                          
                          fluidRow(
                            box(width = 3,
                                selectInput("niveau", "Niveau:", c("Gemeenten" = "Gemeenten",
                                                                   "Wijken" = "Wijken",
                                                                   "Buurten" = "Buurten")),
                                selectInput("gemeente", "Gemeente:", choices=unique(gemeenten$GM_NAAM)),
                                selectInput("wijken", "Wijk:", choices=NULL),
                                selectInput("buurten", "Buurt:", choices=NULL),
                                varSelectInput("variable", "Variabele:", Filter(is.numeric, gemeenten))
                                )
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
                          ),
                          fluidRow(
                            tabBox(width = 12,
                                   # The id lets us use input$tabset1 on the server to find the current tab
                                   id = "tabset1", height = "250px",
                                   tabPanel("Gezondheid en Welzijn", textOutput('table')),
                                   tabPanel("Detailhandel"), textOutput('table'),
                                   tabPanel("Horeca"),
                                   tabPanel("Kinderopvang"),
                                   tabPanel("Onderwijs"),
                                   tabPanel("Verkeer en vervoer"),
                                   tabPanel("Vrije tijd en cultuur")
                            )
                          )
                  ),
                  
                  # Second tab content
                  tabItem(tabName = "Tab2",
                          h2("Voor eventueel ander tab ")
                  )
                )
  )
)
