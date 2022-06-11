ui <- dashboardPage(
  dashboardHeader(title = "Buurtvergelijker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Voorzieningen", tabName = "Voorzieningen", icon = icon("dashboard")),
      menuItem("Gezondheid", tabName = "Gezondheid", icon = icon("th")),
      menuItem("Verkeersongevallen", tabName = "verkeersongevallen", icon = icon("th")),
      menuItem("Criminaliteit", tabName = "Criminaliteit", icon = icon("th")))
  ), # Dashboard sidebar
  dashboardBody(tags$head(tags$style(HTML('.box{box-shadow: none;border-style: none;}.content-wrapper { overflow: auto; }.leaflet-top, .leaflet-bottom {
    z-index: unset !important;
  }'))),
                tabItems(
                  tabItem(
                    tabName = "Voorzieningen",
                    h2("Dashboard nabijheid voorzieningen op gemeente-, wijk- of buurtniveau"),
                    fluidRow(
                      column(width = 3,
                             box(title = "Postcode zoeken?", width = NULL, status = "primary", solidHeader = T, collapsible = T, collapsed = T,
                                 textInput("postcode", "Weet u niet uw exacte gemeente, wijk of buurt? Vul dan hier uw postcode in:"),
                                 textOutput("postcode_info")), # Box postcode zoeken
                             box(title = "Selecteer een niveau", width = NULL, status = "primary", solidHeader = T,
                                 "Selecteer het gewenste niveau, gebied en vergelijkbaarheidsniveau en druk op 'indienen' om door te gaan",
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
                  
                  #####Start gezondheid   
                  
                  tabItem(tabName = "Gezondheid",
                          h2("Dashboard gezondheid op  gemeente-, wijk-, of buurtniveau"),
                          fluidRow(
                            column(width = 3,
                                   box(title = "Uitleg app", width = NULL, status = "primary", solidHeader = T, collapsible = T, collapsed = T,
                                       "In deze app kunt u informatie vinden over de gezondheid in een gebied.", br(),
                                       br(),
                                       "Als eerste moet de box “Niveau en gebied selecteren” worden ingevuld. Hier kan een niveau geselecteerd worden,
                                        een gebied, en met welke andere gebieden u wilt vergelijken. 
                                        Als u tevreden bent met uw keuze, druk dan op “Zoeken”.", br(),
                                       br(),
                                       "Hierna verschijnt een box met informatie over het gekozen gebied. Daarnaast is de leeftijdsopbouw van het 
                                       gebied te zien. Ook ziet u een kaart met uw gekozen gebied (blauwe pointer) en de andere gebieden 
                                       waarmee wordt vergeleken.", br(),
                                       br(),
                                       "Vervolgens kunt u bij “Kies een thema” een thema en subthema kiezen. 
                                        Dan ziet u verschillende visualisaties voor het gekozen subthema. Voor een deel van de visualisaties kunt
                                       u nog kiezen voor welke leeftijdsgroep u de gezondheid wilt zien. "), # Uitleg app
                                   box(title="Niveau en gebied selecteren", width = NULL, status = 'primary', solidHeader = T,
                                       "Selecteer het gewenste niveau, gebied en vergelijkbaarheidsniveau en druk op 'zoeken' om door te gaan.", br(),
                                       br(),
                                       selectInput("niveau_gez", "Niveau:", c("Gemeenten" = "Gemeenten",
                                                                              "Wijken" = "Wijken",
                                                                              "Buurten" = "Buurten")),
                                       conditionalPanel(
                                         condition = "input.niveau_gez == 'Gemeenten'",
                                         selectInput("gemeente1_gez", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente1
                                         selectInput("vergelijkbaar1_gez", "Vergelijken met:", c("Gebieden met dezelfde leeftijdsopbouw" = "age_distribution",
                                                                                               "Alle gebieden in Nederland" = "Nederland", 
                                                                                               "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                               "Gebieden met hetzelfde inkomensniveau" = "Inkomensniveau")) # Select input vergelijkbaar1
                                       ), #conditionalpanel 1
                                       conditionalPanel(
                                         condition = "input.niveau_gez == 'Wijken'",
                                         selectInput("gemeente2_gez", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente2
                                         selectInput("wijken2_gez", "Wijk:", choices = NULL), # Select input wijken2,
                                         selectInput("vergelijkbaar2_gez", "Vergelijken met:", c("Gebieden met dezelfde leeftijdsopbouw" = "age_distribution",
                                                                                               "Alle gebieden in Nederland" = "Nederland", 
                                                                                               "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau",
                                                                                               "Gebieden met hetzelfde inkomensniveau" = "Inkomensniveau")), # Select input vergelijkbaar 2
                                       ), # Conditional panel 2
                                       conditionalPanel(
                                         condition = "input.niveau_gez == 'Buurten'",
                                         selectInput("gemeente3_gez", "Gemeente:", choices = unique(gemeenten$GM_NAAM)), # Select input gemeente3
                                         selectInput("wijken3_gez", "Wijk:", choices = NULL), # Select input wijken3
                                         selectInput("buurten3_gez", "Buurt:", choices = NULL), # Select input buurten3
                                         selectInput("vergelijkbaar3_gez", "Vergelijken met:", c("Gebieden met dezelfde leeftijdsopbouw" = "age_distribution",
                                                                                               "Alle gebieden in Nederland" = "Nederland", 
                                                                                               "Gebieden met hetzelfde stedelijkheidsniveau" = "Stedelijkheidsniveau")) # Select input vergelijkbaar3
                                       ), # Conditional panel 3 buurten
                                       actionButton("action_gez", "Zoeken")
                                   ) #box voor niveau 
                            ), #column 
                            uiOutput("info_box_gez"), # Box informatie
                            uiOutput("age_box_gez"), # Box age distribution
                            uiOutput("kaart_box_gez"), # Box geselecteerde plek
                            
                          ), #fluidrow gezondheid input
                          br(),
                          br(),
                          fluidRow(
                                   box(title = "Kies een thema", width = 3, status = "primary", solidHeader = T,
                                       "Selecteer het gewenste thema en subthema en druk op 'zoeken' om door te gaan.", br(),
                                       br(),
                                       selectInput("thema_gez", "Thema:", c("Gezondheid en beperkingen", "Leefstijl", "Participatie en omgeving") ), 
                                       selectInput("subthema_gez", "Subthema:", choices = NULL),
                                       actionButton("action_thema_gez", "Zoeken")
                                       ),
                            
                                  uiOutput("plots")
                            
                          ), #fluidrow gezondheid plots
                      "Bron data: Gezondheid per wijk en buurt; 2012/2016/2020 (indeling 2020) RIVM"    
                  ), # tab item gezondheidszorg
                  
                  tabItem(tabName = "Verkeersongevallen",
                          h2("Eventueel voor onderwijs")
                  ), # Tab item onderwijs
                  
                  tabItem(tabName = "Criminaliteit",
                          h2("Eventueel voor huizenmarkt")
                  ) # Tab Item huizenmarkt
                ) # TabItems
  ) # Dashboard body
) # Dashboard page
