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
#full_data_crime <- readRDS("../Data/full_data3.rds")
#full_data_crime_norm <- readRDS("../Data/full_data4.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Buurtvergelijker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Voorzieningen", tabName = "Voorzieningen", icon = icon("dashboard")),
      menuItem("Gezondheid", tabName = "Gezondheid", icon = icon("th")),
      menuItem("Criminaliteit", tabName = "Criminaliteit", icon = icon("dashboard")),
      menuItem("Verkeersincidenten", tabName = "Verkeersincidenten", icon = icon("th")))
  ), # Dashboard sidebar
  dashboardBody(tags$head(tags$style(HTML('.box{box-shadow: none;border-style: none;}.content-wrapper { overflow: auto; }'))),
                tabItems(
                  tabItem(
                    tabName = "Voorzieningen",
                    h2("Voorzieningen op gemeente-, wijk- of buurtniveau"),
                    fluidRow(
                      column(width = 3,
                             box(title = "Postcode zoeken?", width = NULL, status = "primary", solidHeader = T, collapsible = T, collapsed = T,
                                 textInput("postcode", "Weet u niet uw precieze gemeente, wijk of buurt? Vul dan hier uw postcode in:"),
                                 textOutput("postcode_info")), # Box postcode zoeken
                             box(title = "Kies een niveau", width = NULL, status = "primary", solidHeader = T,
                                 "Kies het gewenste niveau, gebied en waar u mee wilt vergelijken en druk op 'zoeken' om door te gaan",
                                 selectInput("niveau", "Niveau:", c("Gemeenten" = "Gemeenten",
                                                                    "Wijken" = "Wijken",
                                                                    "Buurten" = "Buurten")), # Select input niveau
                                 conditionalPanel(
                                   condition = "input.niveau == 'Gemeenten'",
                                   selectInput("gemeente1", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente1
                                   selectInput("vergelijkbaar1", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                       "Gebieden met dezelfde stedelijkheid" = "Stedelijkheidsniveau",
                                                                                       "Gebieden met ongeveer hetzelfde inkomen" = "Inkomensniveau",
                                                                                       "Gebieden met ongeveer dezelfde opleiding" = "Opleidingsniveau")) # Select input vergelijkbaar1
                                 ), # Conditional panel 1 gemeenten
                                 conditionalPanel(
                                   condition = "input.niveau == 'Wijken'",
                                   selectInput("gemeente2", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente2
                                   selectInput("wijken2", "Wijk:", choices = NULL), # Select input wijken2,
                                   selectInput("vergelijkbaar2", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                       "Gebieden met dezelfde stedelijkheid" = "Stedelijkheidsniveau",
                                                                                       "Gebieden met ongeveer hetzelfde inkomen" = "Inkomensniveau",
                                                                                       "Gebieden met ongeveer dezelfde opleiding" = "Opleidingsniveau")) # Select input vergelijkbaar 2
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
                      box(title = "Top 5 alle voorzieningen", width = 2, background = "red", 
                          "Top 5 met vergelijkbare gebieden voor alle voorzieningen",
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
                             uiOutput("top5"), # Box top 5 thema
                      ),
                      uiOutput("kaartNL"), # Box kaart
                      uiOutput("box_staafdiagram"),
                    ) # Fluid row 2 histogram, kaart, thema top 5
                  ), # Tab item dashboard
                  
                  
                  tabItem(tabName = "Gezondheid",
                          h2("Eventueel voor gezondheidszorg")
                  ), # tab item gezondheidszorg
                  
                  tabItem(tabName = "Criminaliteit",
                          h2("Criminaliteit: Dashboard misdrijven op gemeente-, wijk-, en buurtniveau"),
                          fluidRow(
                            column(width = 3, 
                                   box(title = "Selecteer niveau", status = "primary", solidHeader = T, width = NULL,
                                       selectInput("niveau_crime", "Niveau:", choices = c("Gemeenten", "Wijken", "Buurten")),
                                       conditionalPanel(
                                         condition = "input.niveau_crime == 'Gemeenten'",
                                         selectInput("gemeente_crime1", "Gemeente:", choices = unique(gemeenten$GM_NAAM)),
                                         selectInput("vergelijkbaar_crime1", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                                   "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                                   "Gebieden met hetzelfde inkomensniveau" = "Inkomensniveau"))),
                                       conditionalPanel(
                                         condition = "input.niveau_crime == 'Wijken'", 
                                         selectInput("gemeente_crime2", "Gemeente:", choices = unique(gemeenten$GM_NAAM)),
                                         selectInput("wijken_crime2", "Wijk:", choices = NULL), 
                                         selectInput("vergelijkbaar_crime2", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                                   "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                                   "Gebieden met hetzelfde inkomensniveau" = "Inkomensniveau"))),
                                       conditionalPanel(
                                         condition = "input.niveau_crime == 'Buurten'", 
                                         selectInput("gemeente_crime3", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), 
                                         selectInput("wijken_crime3", "Wijk:", choices = NULL),
                                         selectInput("buurten_crime3", "Buurt:", choices = NULL), 
                                         selectInput("vergelijkbaar_crime3", "Vergelijken met:", c("Alle gebieden in Nederland" = "Nederland", 
                                                                                                   "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau"))),
                                       
                                       selectInput("aantal_crime", "Aantallen:", choices = c("Aantal misdrijven", "Aantallen per 1000 inwoners")), 
                                       actionButton("action_crime", "Zoeken"))), 
                            column(width = 6, 
                                   box(title = "Grafiek met misdrijven over tijd", status = "warning", solidHeader = T, width = NULL,
                                       plotOutput("crime_plot"))), 
                            column(width = 3, 
                                   box(title = "Uitleg geselecteerde misdaad", status = "warning", solidHeader = T, width = NULL,
                                       textOutput("info_crime")))
                          ),
                          fluidRow(
                            column(width = 3, 
                                   box(title = "Selecteer thema", status = "primary", solidHeader = T, width= NULL,
                                       selectInput("thema_crime", "Thema misdrijf", choices = c("Totaal misdrijven",
                                                                                                "Vermogensdelicten",
                                                                                                "Gewelds- en seksuele misdrijven", 
                                                                                                "Vernielingen en misdrijven tegen openbare orde en gezag", 
                                                                                                "Verkeersmisdrijven", 
                                                                                                "Misdrijven omgeving en milieu", 
                                                                                                "Overige misdrijven")),
                                       selectInput("soort_crime", "Soort misdrijf:", choices = NULL), 
                                       selectInput("jaar_crime", "Jaar:", choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")))), 
                            column(width = 6, 
                                   box(title = "Kaart met misdrijven", status = "warning", solidHeader = T, width = NULL,
                                       leafletOutput("crime_map"))),
                            column(width =3, 
                                   uiOutput("top5_crime")
                                   #box(title = "Top 5 criminaliteit", background = "red", width = NULL,
                                   #"Top 5 met de meeste geselecteerde criminaliteit",
                                   #"Hier komt de algemene top 5 zonder geselecteerd thema",
                                   #tableOutput('top5_crime2'))
                                   
                            )
                          )
                  ), # Tab item criminaliteit
                  
                  tabItem(tabName = "Verkeersincidenten",
                          h2("Eventueel voor huizenmarkt")
                  ) # Tab Item huizenmarkt
                ) # TabItems
  ) # Dashboard body
) # Dashboard page


