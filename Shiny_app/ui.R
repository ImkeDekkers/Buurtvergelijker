library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(htmltools)
library(shinyWidgets)

gemeenten <- readRDS("../Data/gemeenten.rds")
wijken <- readRDS("../Data/wijken.rds")
buurten <- readRDS("../Data/buurten.rds")
postcodes_final <- readRDS("../Data/postcodes_final.rds")
full_data <- readRDS("../Data/full_data.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Buurtvergelijker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Gezondheidszorg", tabName = "Gezondheidszorg", icon = icon("th")),
      menuItem("Onderwijs", tabName = "Onderwijs", icon = icon("th")),
      menuItem("Verkeersveiligheid", tabName = "Verkeersveiligheid", icon = icon("car-side", class = "fa-solid fa-car-side", lib = "font-awesome")))
  ), # Dashboard sidebar
  dashboardBody(tags$head(tags$style(HTML('.box{box-shadow: none;border-style: none;}.content-wrapper { overflow: auto; }'))),
                tabItems(
                  tabItem(
                    tabName = "Dashboard",
                    h2("Dashboard nabijheid voorzieningen op gemeente-, wijk- of buurtniveau"),
                    fluidRow(
                      column(width = 3,
                             box(title = "Postcode zoeken?", width = NULL, status = "primary", solidHeader = T, collapsible = T, collapsed = T,
                                 textInput("postcode", "Weet u niet uw exacte gemeente, wijk of buurt? Vul dan hier uw postcode in:"),
                                 textOutput("postcode_info")), # Box postcode zoeken
                             box(title = "Selecteer een niveau", width = NULL, status = "primary", solidHeader = T,
                                 "Selecteer het gewenste niveau, gebied en vergelijkbaarheidniveau en druk op 'indienen' om door te gaan",
                                 selectInput("niveau", "Niveau:", c("Gemeenten" = "Gemeenten",
                                                                    "Wijken" = "Wijken",
                                                                    "Buurten" = "Buurten")), # Select input niveau
                                 conditionalPanel(
                                   condition = "input.niveau == 'Gemeenten'",
                                   selectInput("gemeente1", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente1
                                   selectInput("vergelijkbaar1", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                       "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                       "Gebieden met hetzelfde inkomensniveau" = "Inkomensniveau",
                                                                                       "Gebieden met hetzelfde opleidingsniveau" = "Opleidingsniveau")) # Select input vergelijkbaar1
                                 ), # Conditional panel 1 gemeenten
                                 conditionalPanel(
                                   condition = "input.niveau == 'Wijken'",
                                   selectInput("gemeente2", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente2
                                   selectInput("wijken2", "Wijk:", choices = NULL), # Select input wijken2,
                                   selectInput("vergelijkbaar2", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                       "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                       "Gebieden met hetzelfde inkomensniveau" = "Inkomensniveau",
                                                                                       "Gebieden met hetzelfde opleidingsniveau" = "Opleidingsniveau")) # Select input vergelijkbaar 2
                                 ), # Conditional panel 2 wijken
                                 conditionalPanel(
                                   condition = "input.niveau == 'Buurten'",
                                   selectInput("gemeente3", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente3
                                   selectInput("wijken3", "Wijk:", choices = NULL), # Select input wijken3
                                   selectInput("buurten3", "Buurt:", choices = NULL), # Select input buurten3
                                   selectInput("vergelijkbaar3", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                       "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau")) # Select input vergelijkbaar3
                                 ), # Conditional panel 3 buurten
                                 actionButton("action", "Indienen")
                             ), # Box selecteer niveau
                      ), # Column
                      box(title = "Informatie over geselecteerd gebied", width = 3, status = "warning", solidHeader = T,
                          "In de onderstaande tabel kan worden afgelezen wat het stedelijkheidsniveau, de inkomensgroep en de opleidingsgroep zijn voor het geselecteerde gebied.",
                          tableOutput("info_area"),
                          "Stedelijkheid: 1 = zeer sterk stedelijk, 5 = niet stedelijk.", br(),
                          "Inkomensniveau: 1 = zeer laag percentage, 4 = hoog percentage van huishoudens met een inkomen onder het sociaal minimum.",br(),
                          "Opleidingsniveau: 1 = zeer laag percentage, 4 = zeer hoog percentage van personen met een lage opleiding.",
                          span(textOutput("ink_vergelijkbaarheid"), style="color:red"),
                          span(textOutput("opl_vergelijkbaarheid"), style="color:red")), # Box informatie
                      box(title = "Geselecteerde plek op de kaart", width = 4, status = "warning", solidHeader = T,
                          "Kaart waarop het gekozen gebied te zien is (blauwe pointer), de top 5 meest vergelijkbare gebieden (rode pointers) en de gebieden waarmee wordt vergeleken.",
                          #"Hier komt de prime map van leaflet met pointer naar centroid van de geselecteerde g/w/b",
                          shinycssloaders::withSpinner(leafletOutput("prime_map"))
                          ), # Box geselecteerde plek
                      box(title = "Top 5 algemeen", width = 2, background = "red", 
                          "Top 5 met vergelijkbare gebieden op basis van alle voorzieningenthema's",
                          #"Hier komt de algemene top 5 zonder geselecteerd thema",
                          tableOutput('top5_algemeen')) # Box top 5 algemeen
                    ), # Fluid row 1 postcode, thema, algemene top 5, niveau, geselecteerde plek
                    fluidRow(
                      column(width = 2,
                             box(title = "Thema", width = NULL, status = "primary", solidHeader = T,
                                 selectInput("thema", "Thema:", c("Gezondheid en welzijn", "Detailhandel", "Horeca", 
                                                                  "Kinderopvang", "Onderwijs", "Verkeer en vervoer", 
                                                                  "Vrije tijd en cultuur")), 
                                 selectInput("subthema", "Subthema:", choices = NULL)), # Box thema
                             box(title = "Top 5 geselecteerd thema", width = NULL, background = "green",
                                 "Top 5 met vergelijkbare gebieden op basis van het gekozen thema",
                                 #"Hier komt de top 5 van vergelijkbare g/w/b voor een bepaald thema",
                                 tableOutput('top5_theme')) # Box top 5 thema
                      ),
                      box(title = "Kaart van Nederland", width = 6, status = "warning", solidHeader = T,
                          "Kaart van Nederland met de geselecteerde vergelijkbare gebieden van het gekozen subthema.",
                          #"Hier komt de kaart van Nederland met geselecteerde vergelijkbare g/w/b op bepaalde variabele",
                          shinycssloaders::withSpinner(leafletOutput("map_variable"))
                          ), # Box kaart
                      uiOutput("box_staafdiagram")
                    ) # Fluid row 2 histogram, kaart, thema top 5
                  ), # Tab item dashboard
                  
                  tabItem(tabName = "Gezondheidszorg",
                          h2("Eventueel voor gezondheidszorg")
                  ), # tab item gezondheidszorg
                  
                  tabItem(tabName = "Onderwijs",
                          h2("Eventueel voor onderwijs")
                  ), # Tab item onderwijs
                  
                  tabItem(tabName = "Verkeersveiligheid",
                          h2("Verkeersongevallen in gemeenten, wijken en buurten van Nederland"),
                          h3("Algemeen overzicht van verkeersongevallen"),
                          fluidRow(
                            column(width = 3,
                                   box(title = "Selecteer het gebied van interesse", width = NULL, status = "primary", solidHeader = T,
                                       selectInput("niveau2", "Niveau:", c("Gemeenten" = "Gemeenten",
                                                                           "Wijken" = "Wijken",
                                                                           "Buurten" = "Buurten")), # Select input niveau2
                                       conditionalPanel(
                                         condition = "input.niveau2 == 'Gemeenten'",
                                         selectInput("gemeente21", "Gemeente:", choices = unique(gemeenten$GM_NAAM)) # Select input gemeente21
                                       ), # Conditional panel 1 gemeenten
                                       conditionalPanel(
                                         condition = "input.niveau2 == 'Wijken'",
                                         selectInput("gemeente22", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente22
                                         selectInput("wijken22", "Wijk:", choices = NULL) # Select input wijken22
                                       ), # Conditional panel 2 wijken
                                       conditionalPanel(
                                         condition = "input.niveau2 == 'Buurten'",
                                         selectInput("gemeente23", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente23
                                         selectInput("wijken23", "Wijk:", choices = NULL), # Select input wijken23
                                         selectInput("buurten23", "Buurt:", choices = NULL) # Select input buurten23
                                       ), # Conditional panel 3 buurten
                                       selectInput("jaar", "Jaar:", c("2020" = "2020",
                                                                       "2019" = "2019",
                                                                       "2018" = "2018",
                                                                       "2017" = "2017"
                                                                       )), # Select input jaar
                                       selectInput("subthema2", "Subthema:", c("Afloop" = "AP3_OMS",
                                                                               "Weersgesteldheid" = "WGD_CODE_1",
                                                                               "Aard" = "AOL_OMS",
                                                                               "Objecttype" = "OTE_OMS",
                                                                               "Wegsituatie" = "WSE_OMS",
                                                                               "Maximum snelheid" = "MAXSNELHD",
                                                                               "Aantal betrokken partijen" = "ANTL_PTJ")),
                                   actionButton("action2", "Zoeken")
                                   ) # Box input
                            ), # Column 1
                            column(width = 5,
                                   box(title = "Algemene trend", width = NULL, status = "danger", solidHeader = T,
                                       "De lijn in de grafiek geeft aan hoeveel ongelukken er hebben plaatsgevonden in het geselecteerde gebied in verschillende jaren.",
                                       shinycssloaders::withSpinner(plotOutput("general_trend"))) # Box algemene trend
                            ), # Column 2
                            column(width = 4,
                                   valueBoxOutput("number_incidents", width = NULL),
                                   valueBoxOutput("stedelijkheid_num", width = NULL)
                            ) # Column 3
                          ), # fluid row algemene trend en aantal
                          h3("Inzicht in subthema in uw buurt"),
                          uiOutput("subtheme_row"),
                          h3("Vergelijk het geselecteerde gebied met andere, vergelijkbare gebieden"),
                          fluidRow(
                            column(width = 4,
                                   box(title = "Top-5 incidenten in vergelijkbare gebieden", width = NULL, status = "success", solidHeader = T,
                                       HTML("In deze tabel wordt de top-5 van gebieden met de meeste incidenten weergegeven. <br>
                                            Staat uw wijk niet in de top-5? Dan ziet u onderaan de tabel op welke rank het geselecteerde gebied staat."),
                                       tableOutput("count_incidents")) # Box top 5
                                   ), # Column top 5
                            column(width = 7,
                                   box(title = "Vergelijking met andere gebieden", width = NULL, status = "success", solidHeader = T,
                                       "Hier kan een vergelijking worden getoond van 2 geselecteerde gebieden",
                                       plotOutput("histogram_incidents")) # Box grafieken of diagram
                            ) # Column vergelijking buurt
                          ) # Fluid row 3 vergelijkbaarheid
                  ) # Tab Item verkeersveiligheid
                ) # TabItems
  ) # Dashboard body
) # Dashboard page

