library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(htmltools)
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
      menuItem("Huizenmarkt", tabName = "Huizenmarkt", icon = icon("th")))
  ), # Dashboard sidebar
  dashboardBody(tags$head(tags$style(HTML('.box{box-shadow: none;border-style: none;}.content-wrapper { overflow: auto; }'))),
                tabItems(
                  tabItem(
                    tabName = "Dashboard",
                    h2("Histogram en kaart van verschillende variabelen op gemeente-, wijk- of buurtniveau"),
                    fluidRow(
                      column(width = 3,
                             box(title = "Postcode zoeken?", width = NULL, status = "primary", solidHeader = T,
                                 textInput("postcode", "Weet u niet uw exacte gemeente, wijk of buurt? Vul dan hier uw postcode in:"),
                                 textOutput("postcode_info")), # Box postcode zoeken
                             box(title = "Thema", width = NULL, status = "primary", solidHeader = T,
                                 selectInput("thema", "Thema:", c("Gezondheid en welzijn", "Detailhandel", "Horeca", 
                                                                 "Kinderopvang", "Onderwijs", "Verkeer en vervoer", 
                                                                 "Vrije tijd en cultuur"))), # Box thema
                      ), # Column
                      box(title = "Selecteer een niveau", width = 3, status = "primary", solidHeader = T,
                          selectInput("niveau", "Niveau:", c("Gemeenten" = "Gemeenten",
                                                            "Wijken" = "Wijken",
                                                            "Buurten" = "Buurten")), # Select input niveau
                          conditionalPanel(
                            condition = "input.niveau == 'Gemeenten'",
                            selectInput("gemeente1", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente1
                            selectInput("vergelijkbaar1", "Vergelijkbaar:", c("Stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                              "Inkomensniveau" = "Inkomensniveau",
                                                                              "Opleidingsniveau" = "Opleidingsniveau",
                                                                              "Nederland" = "Nederland")) # Select input vergelijkbaar1
                          ), # Conditional panel 1 gemeenten
                          conditionalPanel(
                            condition = "input.niveau == 'Wijken'",
                            selectInput("gemeente2", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente2
                            selectInput("wijken2", "Wijk:", choices = NULL), # Select input wijken2,
                            selectInput("vergelijkbaar2", "Vergelijkbaar:", c("Stedelijkheidsiveau" = "Stedelijkheidsniveau",
                                                                              "Inkomensniveau" = "Inkomensniveau",
                                                                              "Opleidingsniveau" = "Opleidingsniveau",
                                                                              "Nederland" = "Nederland")), # Select input vergelijkbaar 2
                          ), # Conditional panel 2 wijken
                          conditionalPanel(
                            condition = "input.niveau == 'Buurten'",
                            selectInput("gemeente3", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente3
                            selectInput("wijken3", "Wijk:", choices = NULL), # Select input wijken3
                            selectInput("buurten3", "Buurt:", choices = NULL), # Select input buurten3
                            selectInput("vergelijkbaar3", "Vergelijkbaar:", c("Stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                              "Nederland" = "Nederland")) # Select input vergelijkbaar3
                          ) # Conditional panel 3 buurten
                      ), # Box selecteer niveau
                      box(title = "Geselecteerde plek op de kaart", width = 4, status = "warning", solidHeader = T,
                          "Hier komt de prime map van leaflet met pointer naar centroid van de geselecteerde g/w/b",
                          leafletOutput("prime_map")), # Box geselecteerde plek
                      box(title = "Top 5 algemeen", width = 2, background = "red", 
                          "Hier komt de algemene top 5 zonder geselecteerd thema",
                          tableOutput('table')) # Box top 5 algemeen
                    ), # Fluid row 1 postcode, thema, algemene top 5, niveau, geselecteerde plek
                    fluidRow(
                      box(title = "Kaart van Nederland", width = 6, status = "warning", solidHeader = T,
                          "Hier komt de kaart van Nederland met geselecteerde vergelijkbare g/w/b op bepaalde variabele",
                          leafletOutput("map_huisarts")), # Box kaart
                      box(title = "Top 5 geselecteerd thema", width = 2, background = "red",
                          "Hier komt de top 5 van vergelijkbare g/w/b voor een bepaald thema"), # Box top 5 thema
                      box(title = "Staafdiagram", width = 4, status = "warning", solidHeader = T,
                          "Hier komt een staafdiagram om je wijk te vergelijken met het gemiddelde van vergelijkbare wijken",
                          plotOutput("plot_huisarts")) # Box staafdiagram
                    ) # Fluid row 2 histogram, kaart, thema top 5
                  ), # Tab item dashboard
                  
                  tabItem(tabName = "Gezondheidszorg",
                          h2("Eventueel voor gezondheidszorg")
                  ), # tab item gezondheidszorg
                  
                  tabItem(tabName = "Onderwijs",
                          h2("Eventueel voor onderwijs")
                  ), # Tab item onderwijs
                  
                  tabItem(tabName = "Huizenmarkt",
                          h2("Eventueel voor huizenmarkt")
                  ) # Tab Item huizenmarkt
                ) # TabItems
  ) # Dashboard body
) # Dashboard page

