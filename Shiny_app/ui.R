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
  
  dashboardBody(tags$head(tags$style(HTML('.box{box-shadow: none;border-style: none;}.content-wrapper { overflow: auto; }'))),
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
                                conditionalPanel(
                                  condition = "input.niveau == 'Gemeenten'",
                                  selectInput("gemeente1", "Gemeente:", choices=unique(gemeenten$GM_NAAM)),
                                  selectInput("vergelijkbaar1", "Vergelijkbaarheid:", c("Stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                        "Inkomensniveau" = "Inkomensniveau",
                                                                                        "Opleidingsniveau" = "Opleidingsniveau"))
                                ),
                                conditionalPanel(
                                  condition = "input.niveau == 'Wijken'",
                                  selectInput("gemeente2", "Gemeente:", choices=unique(gemeenten$GM_NAAM)),
                                  selectInput("wijken2", "Wijk:", choices=NULL),
                                  selectInput("vergelijkbaar2", "Vergelijkbaarheid:", c("Stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                        "Inkomensniveau" = "Inkomensniveau",
                                                                                        "Opleidingsniveau" = "Opleidingsniveau")),
                                  textOutput('ink_vergelijkbaarheid'),
                                  textOutput('opl_vergelijkbaarheid')
                                ),
                                conditionalPanel(
                                  condition = "input.niveau == 'Buurten'",
                                  selectInput("gemeente3", "Gemeente:", choices=unique(gemeenten$GM_NAAM)),
                                  selectInput("wijken3", "Wijk:", choices=NULL),
                                  selectInput("buurten3", "Buurt:", choices=NULL),
                                  selectInput("vergelijkbaar3", "Vergelijkbaarheid:", c("Stedelijkheidsniveau" = "Stedelijkheidsniveau"))
                                )
                                #,
                                #varSelectInput("variable", "Variabele:", Filter(is.numeric, gemeenten))
                            )
                          ),
                          # I think we don't need this fluidRow
                          #fluidRow(
                            #box(title = "Histogram", 
                                #"Wanneer u een niveau en variabele heeft geselecteerd, kunt u in deze grafiek zien hoe de verdeling van deze variabele is op het geselecteerde niveau", br(), 
                                #br(),
                                #plotOutput("histogram")),
                            #box(title = "Kaart", 
                                #"Wanneer u een niveau en variabele heeft geselecteerd, kunt u op deze kaart zien welke waarde van deze variabele hoort bij elke gemeente/wijk/buurt", br(), 
                                #br(),
                                #leafletOutput("map"))
                          #),
                          fluidRow(
                            tabBox(
                                   # The id lets us use input$tabset1 on the server to find the current tab
                                   id = "tabset1", height = "250px",
                                   tabPanel("Gezondheid en Welzijn", plotOutput('plot_huisarts'), leafletOutput('map_huisarts'), plotOutput('plot_ziekenhuis_incl'), leafletOutput("map_ziekenhuizen_incl"), plotOutput('plot_ziekenhuis_excl'), leafletOutput("map_ziekenhuizen_excl")),
                                   tabPanel("Detailhandel", plotOutput('plot_supermarkt'), leafletOutput('map_supermarkt'), plotOutput('plot_ov_levensm'), leafletOutput('map_ov_levensm'), plotOutput('plot_warenhuis'), leafletOutput('map_warenhuis')), 
                                   tabPanel("Horeca", plotOutput('plot_cafes'), leafletOutput('map_cafes'), plotOutput('plot_cafetaria'), leafletOutput('map_cafetaria'), plotOutput('plot_restaurants'), leafletOutput('map_restaurants'), plotOutput('plot_hotels'), leafletOutput('map_hotels')),
                                   tabPanel("Kinderopvang", plotOutput('plot_kinderdagverblijf'), leafletOutput('map_kinderdagverblijf'), plotOutput('plot_opvang'), leafletOutput('map_opvang')),
                                   tabPanel("Onderwijs", plotOutput('plot_basisscholen'), leafletOutput('map_basisscholen'), plotOutput('plot_vo'), leafletOutput('map_vo'), plotOutput('plot_VMBO'), leafletOutput('map_VMBO'), plotOutput('plot_HAVO_VWO'), leafletOutput('map_HAVO_VWO')),
                                   tabPanel("Verkeer en vervoer"),
                                   tabPanel("Vrije tijd en cultuur", plotOutput('plot_bioscoop'), leafletOutput('map_bioscoop'), plotOutput('plot_attractie'), leafletOutput('map_attractie'), plotOutput('plot_podiumkunsten'), leafletOutput('map_podiumkunsten'), plotOutput('plot_musea'), leafletOutput('map_musea'))
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
