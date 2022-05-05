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
                                   tabPanel("Gezondheid en Welzijn", plotOutput('plot_huisarts'), plotOutput('plot_ziekenhuis_incl'), plotOutput('plot_ziekenhuis_excl')),
                                   tabPanel("Detailhandel", plotOutput('plot_supermarkt'), plotOutput('plot_ov_levensm'), plotOutput('plot_warenhuis')), 
                                   tabPanel("Horeca", plotOutput('plot_cafes'), plotOutput('plot_cafetaria'), plotOutput('plot_restaurants'), plotOutput('plot_hotels')),
                                   tabPanel("Kinderopvang", plotOutput('plot_kinderdagverblijf'), plotOutput('plot_opvang')),
                                   tabPanel("Onderwijs", plotOutput('plot_basisscholen'), plotOutput('plot_vo'), plotOutput('plot_VMBO'), plotOutput('plot_HAVO_VWO')),
                                   tabPanel("Verkeer en vervoer"),
                                   tabPanel("Vrije tijd en cultuur", plotOutput('plot_bioscoop'), plotOutput('plot_attractie'), plotOutput('plot_podiumkunsten'), plotOutput('plot_musea'))
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
