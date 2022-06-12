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
      menuItem("Voorzieningen", tabName = "Voorzieningen", icon = icon("building", class = "fa-solid fa-building", lib = "font-awesome")),
      menuItem("Gezondheid", tabName = "Gezondheid", icon = icon("th")),
      menuItem("Verkeersongevallen", tabName = "Verkeersongevallen", icon = icon("car-side", class = "fa-solid fa-car-side", lib = "font-awesome")),
      menuItem("Criminaliteit", tabName = "Criminaliteit", icon = icon("th")))
  ), # Dashboard sidebar
  dashboardBody(tags$head(tags$style(HTML('.box{box-shadow: none;border-style: none;}.content-wrapper { overflow: auto; }'))),
                tabItems(
                  tabItem(
                    tabName = "Voorzieningen",
                    h2("Voorzieningen op gemeente-, wijk- of buurtniveau"),
                    fluidRow(
                      column(width = 3,
                             box(title = "Uitleg app", width = NULL, status = "primary", solidHeader = T, collapsible = T, collapsed = T,
                                 "In deze app kunt u informatie vinden over de afstand tot voorzieningen in een gebied.", br(),
                                 br(),
                                 "Als eerste moet de box “Kies een niveau” worden ingevuld. Hier kan een niveau geselecteerd worden,
                                 een gebied, en met welke andere gebieden u wilt vergelijken. Weet u niet in welke wijk of buurt u woont? 
                                 Dan kunt u eerst nog uw postcode invullen bij “Postcode zoeken?”. 
                                 Als u tevreden bent met uw keuze, druk dan op “Zoeken”.", br(),
                                 br(),
                                 "Hierna verschijnt een box met informatie over het gekozen gebied. Ook ziet u een kaart met uw gekozen 
                                 gebied (blauwe pointer) en de andere gebieden waarmee wordt vergeleken. 
                                 Als laatste ziet u nog een lijst met de 5 gebieden die het meest op uw gebied lijken. 
                                 Dit is berekend op basis van de afstand tot voorzieningen en het aantal voorzieningen binnen een bepaalde afstand. 
                                 Deze gebieden zijn in de kaart te zien met de rode pointers.", br(),
                                 br(),
                                 "Vervolgens kunt u bij “Thema” een thema en subthema kiezen. 
                                 Dan ziet u een kaart met de afstand tot de gekozen voorziening voor het gekozen gebied (blauwe pointer) 
                                 en voor alle gebieden waarmee wordt vergeleken. Ook is opnieuw een lijst te zien met de 5 gebieden 
                                 die het meest op uw gebied lijken. Dit keer alleen op basis van de voorzieningen in het gekozen thema. 
                                 Deze gebieden zijn in de kaart te zien met de groene pointers. Als laatste ziet u nog een staafdiagram
                                 met hoeveel voorzieningen er binnen een bepaalde afstand zitten. "), # Uitleg app
                             box(title = "Postcode zoeken?", width = NULL, status = "primary", solidHeader = T, collapsible = T, collapsed = T,
                                 textInput("postcode", "Weet u niet uw precieze gemeente, wijk of buurt? Vul dan hier uw postcode in:"),
                                 textOutput("postcode_info")), # Box postcode zoeken
                             box(title = "Kies een niveau", width = NULL, status = "primary", solidHeader = T,
                                 "Kies het gewenste niveau, gebied en waar u mee wilt vergelijken en druk op 'zoeken' om door te gaan.",
                                 selectInput("niveau", "Niveau:", c("Gemeenten" = "Gemeenten",
                                                                    "Wijken" = "Wijken",
                                                                    "Buurten" = "Buurten")), # Select input niveau
                                 conditionalPanel(
                                   condition = "input.niveau == 'Gemeenten'",
                                   selectInput("gemeente1", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente1
                                   selectInput("vergelijkbaar1", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                       "Gebieden met dezelfde stedelijkheid" = "Stedelijkheidsniveau",
                                                                                       "Gebieden met ongeveer hetzelfde inkomen" = "Inkomensniveau")) # Select input vergelijkbaar1
                                 ), # Conditional panel 1 gemeenten
                                 conditionalPanel(
                                   condition = "input.niveau == 'Wijken'",
                                   selectInput("gemeente2", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente2
                                   selectInput("wijken2", "Wijk:", choices = NULL), # Select input wijken2,
                                   selectInput("vergelijkbaar2", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                       "Gebieden met dezelfde stedelijkheid" = "Stedelijkheidsniveau",
                                                                                       "Gebieden met ongeveer hetzelfde inkomen" = "Inkomensniveau")) # Select input vergelijkbaar 2
                                 ), # Conditional panel 2 wijken
                                 conditionalPanel(
                                   condition = "input.niveau == 'Buurten'",
                                   selectInput("gemeente3", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente3
                                   selectInput("wijken3", "Wijk:", choices = NULL), # Select input wijken3
                                   selectInput("buurten3", "Buurt:", choices = NULL), # Select input buurten3
                                   selectInput("vergelijkbaar3", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                       "Gebieden met dezelfde stedelijkheid" = "Stedelijkheidsniveau")) # Select input vergelijkbaar3
                                 ), # Conditional panel 3 buurten
                                 actionButton("action", "Zoeken")
                             ), # Box selecteer niveau
                      ), # Column
                      uiOutput("info_box"), # Box informatie
                      uiOutput("kaart_box"), # Box geselecteerde plek
                      uiOutput("top5_all") # Box top 5 algemeen
                    ), # Fluid row 1 postcode, thema, algemene top 5, niveau, geselecteerde plek
                    fluidRow(
                      column(width = 2,
                             box(title = "Thema", width = NULL, status = "primary", solidHeader = T,
                                 selectInput("thema", "Thema:", c("Gezondheid en welzijn", "Detailhandel", "Horeca", 
                                                                  "Kinderopvang", "Onderwijs", "Verkeer en vervoer", 
                                                                  "Vrije tijd en cultuur")), 
                                 selectInput("subthema", "Subthema:", choices = NULL),
                                 actionButton("action_theme", "Zoeken")), # Box thema
                             uiOutput("top5"), # Box top 5 thema
                      ),
                      uiOutput("kaartNL"), # Box kaart
                      uiOutput("box_staafdiagram"),
                    ), # Fluid row 2 histogram, kaart, thema top 5
                    "Bron data: Kerncijfers wijken en buurten 2020 CBS"
                  ), # Tab item dashboard
                  
                  tabItem(tabName = "Gezondheid",
                          h2("Eventueel voor gezondheidszorg")
                  ), # tab item gezondheidszorg
                  
                  tabItem(tabName = "Verkeersongevallen",
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
                            column(width = 6,
                                   box(title = "Vergelijking met andere gebieden", width = NULL, status = "success", solidHeader = T,
                                       "Hier kan een vergelijking worden getoond van 2 geselecteerde gebieden",
                                       plotOutput("histogram_incidents")) # Box grafieken of diagram
                            ) # Column vergelijking buurt
                          ) # Fluid row 3 vergelijkbaarheid
                  ), # Tab Item verkeersveiligheid
                  tabItem(tabName = "Criminaliteit",
                          h2("Eventueel voor huizenmarkt")
                  ) # Tab Item huizenmarkt
                ) # TabItems
  ) # Dashboard body
) # Dashboard page

