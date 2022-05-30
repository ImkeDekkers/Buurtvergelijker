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
full_data_crime <- readRDS("../Data/full_data3.rds")
full_data_crime_norm <- readRDS("../Data/full_data4.rds")


ui <- dashboardPage(
  dashboardHeader(title = "Buurtvergelijker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Gezondheidszorg", tabName = "Gezondheidszorg", icon = icon("th")),
      menuItem("Criminaliteit", tabName = "Criminaliteit", icon = icon("dashboard")),
      menuItem("Huizenmarkt", tabName = "Huizenmarkt", icon = icon("th")))
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
                                 tableOutput('top5_theme')), # Box top 5 thema
                      ),
                      box(title = "Kaart van Nederland", width = 6, status = "warning", solidHeader = T,
                          "Kaart van Nederland met de geselecteerde vergelijkbare gebieden van het gekozen subthema.",
                          #"Hier komt de kaart van Nederland met geselecteerde vergelijkbare g/w/b op bepaalde variabele",
                          shinycssloaders::withSpinner(leafletOutput("map_variable"))
                          ), # Box kaart
                      uiOutput("box_staafdiagram"),
                    ) # Fluid row 2 histogram, kaart, thema top 5
                  ), # Tab item dashboard
                  
                  tabItem(tabName = "Gezondheidszorg",
                          h2("Eventueel voor gezondheidszorg")
                  ), # tab item gezondheidszorg
                  
                  tabItem(tabName = "Criminaliteit",
                          h2("Criminaliteit: Dashboard misdrijven op gemeente-, wijk-, en buurtniveau"),
                          fluidRow(
                            column(width = 3, 
                                   box(title = "Selecteer niveau", status = "primary", solidHeader = T, 
                                       selectInput("niveau_crime", "Niveau:", choices = c("Gemeenten", "Wijken", "Buurten")),
                                       conditionalPanel(
                                         condition = "input.niveau_crime == 'Gemeenten'",
                                         selectInput("gemeente_crime1", "Gemeente:", choices = unique(gemeenten$GM_NAAM)),
                                         selectInput("vergelijkbaar_crime1", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                                   "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                                   "Gebieden met hetzelfde inkomensniveau" = "Inkomensniveau",
                                                                                                   "Gebieden met hetzelfde opleidingsniveau" = "Opleidingsniveau"))),
                                       conditionalPanel(
                                         condition = "input.niveau_crime == 'Wijken'", 
                                         selectInput("gemeente_crime2", "Gemeente:", choices = unique(gemeenten$GM_NAAM)),
                                         selectInput("wijken_crime2", "Wijk:", choices = NULL), 
                                         selectInput("vergelijkbaar_crime2", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                                   "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                                   "Gebieden met hetzelfde inkomensniveau" = "Inkomensniveau",
                                                                                                   "Gebieden met hetzelfde opleidingsniveau" = "Opleidingsniveau"))),
                                       conditionalPanel(
                                         condition = "input.niveau_crime == 'Buurten'", 
                                         selectInput("gemeente_crime3", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), 
                                         selectInput("wijken_crime3", "Wijk:", choices = NULL),
                                         selectInput("buurten_crime3", "Buurt:", choices = NULL), 
                                         selectInput("vergelijkbaar3", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                             "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau"))),
                                       
                                       selectInput("aantal_crime", "Aantallen:", choices = c("Aantal misdrijven", "Aantallen per 1000 inwoners")), 
                                       actionButton("action_crime", "Zoeken"))), 
                            column(width = 9, 
                                   box(title = "Grafiek met misdrijven over tijd", status = "primary", solidHeader = T, 
                                       plotOutput("crime_plot")))),
                          fluidRow(
                            column(width = 3, 
                                   box(title = "Selecteer niveau", status = "primary", solidHeader = T, 
                                       selectInput("soort_crime", "Type Misdrijf:", choices = c("Totaal misdrijven", 
                                                                                                "Diefstal/inbraak woning", 
                                                                                                "Diefstal/inbraak box/garage/schuur", 
                                                                                                "Diefstal uit/vanaf motorvoertuigen",
                                                                                                "Diefstal van motorvoertuigen",
                                                                                                "Diefstal van brom-, snor-, fietsen",
                                                                                                "Zakkenrollerij", 
                                                                                                "Diefstal af/uit/van overige voertuigen", 
                                                                                                "Ongevallen (weg)", 
                                                                                                "Zedenmisdrijf", 
                                                                                                "Moord, doodslag", 
                                                                                                "Openlijk geweld (persoon)", 
                                                                                                "Bedreiging", 
                                                                                                "Mishandeling", 
                                                                                                "Straatroof", 
                                                                                                "Overval", 
                                                                                                "Diefstallen (water)", 
                                                                                                "Brand/ontploffing", 
                                                                                                "Overige vermogensdelicten",
                                                                                                "Mensenhandel",
                                                                                                "Drugs/drankoverlast", 
                                                                                                "Vernieling cq. zaakbeschadiging", 
                                                                                                "Burengerucht (relatieproblemen)", 
                                                                                                "Huisvredebreuk", 
                                                                                                "Diefstal/inbraak bedrijven", 
                                                                                                "Winkeldiefstal", 
                                                                                                "Inrichting Wet Milieubeheer", 
                                                                                                "Bodem",
                                                                                                "Water", 
                                                                                                "Afval", 
                                                                                                "Bouwstoffen", 
                                                                                                "Mest", 
                                                                                                "Transport gevaarlijke stoffen", 
                                                                                                "Vuurwerk", 
                                                                                                "Bestrijdingsmiddelen", 
                                                                                                "Natuur en landschap", 
                                                                                                "Ruimtelijke ordening", 
                                                                                                "Dieren", 
                                                                                                "Voedselveiligheid", 
                                                                                                "Bijzondere wetten", 
                                                                                                "Leefbaarheid (overig)", 
                                                                                                "Drugshandel", 
                                                                                                "Mensensmokkel", 
                                                                                                "Wapenhandel", 
                                                                                                "Kinderporno", 
                                                                                                "Kinderprostitutie", 
                                                                                                "Onder invloed (lucht)", 
                                                                                                "Lucht (overig)" , 
                                                                                                "Onder invloed (water)", 
                                                                                                "Onder invloed (weg)", 
                                                                                                "Weg (overig)", 
                                                                                                "Aantasting openbare orde", 
                                                                                                "Discriminatie", 
                                                                                                "Vreemdelingenzorg", 
                                                                                                "Maatschappelijke intergriteit (overig)", 
                                                                                                "Cybercrime", 
                                                                                                "Horizontale fraude", 
                                                                                                "Verticale fraude", 
                                                                                                "Fraude (overig)")), 
                                       selectInput("jaar_crime", "Jaar:", choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")))), 
                            column(width = 9, 
                                   box(title = "Kaart met misdrijven", status = "primary", solidHeader = T,
                                       leafletOutput("crime_map")
                                   )
                            )
                          )
                  ), # Tab item criminaliteit
                  
                  tabItem(tabName = "Huizenmarkt",
                          h2("Eventueel voor huizenmarkt")
                  ) # Tab Item huizenmarkt
                ) # TabItems
  ) # Dashboard body
) # Dashboard page

