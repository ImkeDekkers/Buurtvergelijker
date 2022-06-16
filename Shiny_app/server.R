# Load data
#gemeenten <- readRDS("../Data/gemeenten.rds")
#wijken <- readRDS("../Data/wijken.rds")
#buurten <- readRDS("../Data/buurten.rds")
postcodes_final <- readRDS("../Data/postcodes_final.rds")
full_data <- readRDS("../Data/full_data.rds")
source("../Functions/plot4_facilities.R")
source("../Functions/create_map_facilities.R")
source("../Functions/top5_facilities.R")
source("../Functions/postcode_lookup.R")
source("../Functions/Dataset.R")

gezondheid_all <-readRDS("../Data/gezondheid_all.rds")
source("../Functions/SimilarAgeDistribution.R")
source("../Functions/HealthPlots.R")

all_polygons <- full_data %>% 
  select(BU_CODE, BU_NAAM, WK_CODE, WK_NAAM, GM_CODE, GM_NAAM, geometry, 
         centroid, Niveau, centroidx, centroidy, `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
intersection <- readRDS("../Data/intersection.rds")


shinyServer(function(input, output, session) {
  
    # Finding gemeente, wijk and buurt name based on the input postcode
    # Uses function postcode_lookup from file postcode_lookup
    output$postcode_info <- renderText(postcode_lookup(postcodes_final,input$postcode))
  
    # Make selection dependent on previous input
    observeEvent(input$gemeente2, {
      updateSelectInput(session, 'wijken2',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente2]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$gemeente3, {
      updateSelectInput(session, 'wijken3',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente3]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$wijken3,{
      updateSelectInput(session, 'buurten3',
                        choices = unique(postcodes_final$buurtnaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente3 & 
                                                                         postcodes_final$wijknaam2020==input$wijken3]))       # Only display buurten that are in the selected wijk
    })
    observeEvent(input$thema, {
      if (input$thema == "Gezondheid en welzijn") {
        updateSelectInput(session, 'subthema', 
                          choices = c("Huisartsenpraktijk", "Ziekenhuis incl. buitenpolikliniek","Ziekenhuis excl. buitenpolikliniek","Apotheek"))
      }else if (input$thema == "Detailhandel") {
        updateSelectInput(session, 'subthema', 
                          choices = c("Supermarkt", "Overige dagelijkse levensmiddelen", "Warenhuis"))
      }else if (input$thema == "Horeca") {
        updateSelectInput(session, 'subthema', 
                          choices = c("Cafe", "Cafetaria", "Restaurant", "Hotel"))
      }else if (input$thema == "Kinderopvang") {
        updateSelectInput(session, 'subthema', 
                          choices = c("Kinderdagverblijf", "Buitenschoolse opvang"))
      }else if (input$thema == "Onderwijs") {
        updateSelectInput(session, 'subthema', 
                          choices = c("Basisschool", "Voortgezet onderwijs", "VMBO school", "HAVO/VWO school"))
      }else if (input$thema == "Verkeer en vervoer") {
        updateSelectInput(session, 'subthema', 
                          choices = c("Oprit hoofdverkeersweg","Treinstation","Belangrijk overstapstation")) 
        }else if (input$thema == "Vrije tijd en cultuur") {
                            updateSelectInput(session, 'subthema', 
                                              choices = c("Bioscoop", "Attractie", "Podiumkunsten", "Museum",
                                                          "Zwembad", "Kunstijsbaan", "Bibliotheek", "Poppodium","Sauna","Zonnebank"))
        }
          })
    

    # used datafor the other visualizations, depends on selected level, area and comparable areas
    # Uses function dataset from the file Dataset
    datasetInput <- eventReactive(input$action,{
      dataset <- dataset(full_data, input$niveau, input$vergelijkbaar1, input$gemeente1,
              input$vergelijkbaar2, input$gemeente2, input$wijken2,
              input$vergelijkbaar3, input$gemeente3, input$wijken3, input$buurten3)$dataset
      selected_polygon <- dataset(full_data, input$niveau, input$vergelijkbaar1, input$gemeente1,
                                  input$vergelijkbaar2, input$gemeente2, input$wijken2,
                                  input$vergelijkbaar3, input$gemeente3, input$wijken3, input$buurten3)$selected_polygon
      ink_level <- dataset(full_data, input$niveau, input$vergelijkbaar1, input$gemeente1,
                           input$vergelijkbaar2, input$gemeente2, input$wijken2,
                           input$vergelijkbaar3, input$gemeente3, input$wijken3, input$buurten3)$ink_level
      if (ink_level=="onbekend"){
      output$ink_vergelijkbaarheid <- renderText(
        print("Let op, door een missende waarde van het inkomensniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
      )}
      else if(ink_level=="bekend"){
        output$ink_vergelijkbaarheid <- renderText(
          print("")
        )}
      
      list_return <- list("dataset" = dataset, "selected_polygon" = selected_polygon)
      return(list_return)
    })
    
    # Get information about selected area in table
    output$info_area <- renderTable({
      df <- as.data.frame(datasetInput()$selected_polygon) %>% 
        select(c("Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)", "inkomengroep"))
      df <- rename(df, "Stedelijkheid"= `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
      df
      }) 

    #Gives table output with 5 most similar areas based on all voorzieningen variables
    output$top5_algemeen <- renderTable(
      table_top5_distances_overall(datasetInput()$dataset, 
                                   input$niveau),
      rownames = TRUE
    )
    
    #Selected theme name for the box titles (changes only when 'zoeken' button is clicked)
    selected_theme_title <- eventReactive(input$action_theme,{
      theme <- input$thema
      return(theme)
    })
    
    # Make icon for maps
    iconblue <- makeAwesomeIcon(
      icon = "arrow-down",
      iconColor = "black",
      markerColor = "blue",
      library = "fa")
    
    iconred <- makeAwesomeIcon(
      icon = "arrow-down",
      iconColor = "black",
      markerColor = "red",
      library = "fa")
    
    icongreen <- makeAwesomeIcon(
      icon = "arrow-down",
      iconColor = "black",
      markerColor = "green",
      library = "fa")
    
    #returns table with top 5 similar areas based on chosen theme (changes only when 'zoeken' button is clicked)
    table_top5_theme <- eventReactive(input$action_theme,{
      table_top5_distances_theme(datasetInput()$dataset, 
                                 input$niveau, 
                                 input$thema)
    })
    #returns table with top 5 similar areas based on chosen theme
    output$top5_theme <- renderTable(
      table_top5_theme(),
      rownames = TRUE
    )
    
    # Create map to point to the selected location and comparable polygons
    output$prime_map <- renderLeaflet({
      leaflet(datasetInput()$dataset) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(color = "navy", weight = 1, 
                    highlightOptions = highlightOptions(color = "black", 
                                                        weight = 2),
                    label = ~htmlEscape(datasetInput()$dataset$NAAM)) %>% 
        addAwesomeMarkers(lng = datasetInput()$dataset$centroidxx,
                          lat = datasetInput()$dataset$centroidyy,
                          icon = iconblue) %>% 
        addAwesomeMarkers(data = top5_distances_overall(datasetInput()$dataset),
                          lng = ~centroidx,
                          lat = ~centroidy,
                          icon = iconred,
                          label = ~NAAM)
    })

    #Selected sutheme name for the box titles (changes only when 'zoeken' button is clicked)
    selected_subtheme_title <- eventReactive(input$action_theme,{
      subthema <- input$subthema
      return(subthema)
    })
    
 
    #Maps for all variables with distance to closest spot, (changes only when 'zoeken' button is clicked)
    # Uses map_subtheme function from create_map_facilities file
    leaflet_map <- eventReactive(input$action_theme,{
      subthema <-  selected_subtheme_title()
      leafl <- map_subtheme(datasetInput()$dataset, subthema, input$niveau, input$thema)
      return(leafl)
    })
    output$map_variable <- renderLeaflet({
        leaflet_map()
    })
    

    #Plot for the amount of instances inside a radius (changes only when 'zoeken' button is clicked)
    # Uses plot_4_theme function from plot4_facilities file
    plot4_map <- eventReactive(input$action_theme,{
      subthema <-  selected_subtheme_title()
      plot <- plot_4_theme(datasetInput()$dataset, subthema)
      return(plot)
    })
    output$plot_variable <- renderPlot({
      plot4_map()
    })
     
    #Selected area name for the box titles (changes only when 'zoeken' button is clicked)
    selected_area_title <- eventReactive(input$action,{
      if(input$niveau=="Gemeenten"){
        title <- input$gemeente1
      }else if(input$niveau=="Wijken"){
        title <- input$wijken2
      }else if(input$niveau=="Buurten"){
        title <- input$buurten3
      }
      return(title)
    })
    
    #Info box, title changes based on the selected area
    output$info_box = renderUI({
      title <- paste0("Informatie over: ", selected_area_title())
      box(title = title, width = 3, status = "warning", solidHeader = T,
            "In de tabel hieronder ziet u wat de stedelijkheid, de inkomensgroep en de opleidingsgroep zijn voor het gekozen gebied.",
            tableOutput("info_area"),
            "Stedelijkheid: 1 = zeer sterk stedelijk, 5 = niet stedelijk.", br(),
            "Inkomensgroep: 1 = klein aandeel onder sociaal minimum, 4 = groot aandeel onder sociaal minimum.",
            span(textOutput("ink_vergelijkbaarheid"), style="color:red")) # Box informatie 
    })
    
    #Kaart box, title changes based on the selected area
    output$kaart_box = renderUI({
      title <- title <- paste0("Kaart met: ", selected_area_title())
      box(title = title, width = 4, status = "warning", solidHeader = T,
          "Kaart waarop het gekozen gebied te zien is (blauwe pointer), de top 5 meest vergelijkbare gebieden (rode pointers) en de gebieden waarmee wordt vergeleken.",
          #"Hier komt de prime map van leaflet met pointer naar centroid van de geselecteerde g/w/b",
          shinycssloaders::withSpinner(leafletOutput("prime_map"))) 
    })
    
    # Box top 5 algemeen
    output$top5_all = renderUI({
    title <- paste0("Kaart met: ", selected_area_title())
    box(title = "Top 5 alle voorzieningen", width = 2, background = "red", 
        "Top 5 met vergelijkbare gebieden voor alle voorzieningen",
        #"Hier komt de algemene top 5 zonder geselecteerd thema",
        tableOutput('top5_algemeen')) 
    })

    #Top 5 voor thema, title changes based on the selected theme
    output$top5 = renderUI({
      title <- paste0("Top 5 ",selected_theme_title())
      box(title = title, width = NULL, background = "green",
          "Top 5 met vergelijkbare gebieden voor het gekozen thema",
          #"Hier komt de top 5 van vergelijkbare g/w/b voor een bepaald thema",
          tableOutput('top5_theme')) 
    })
    
    #Top 5 voor thema, title changes based on the selected theme
    output$kaartNL = renderUI({
      title <- paste0("Kaart van Nederland: ", selected_subtheme_title())
      box(title = title, width = 6, status = "warning", solidHeader = T,
          "Kaart van Nederland met het gekozen subthema.",
          #"Hier komt de kaart van Nederland met geselecteerde vergelijkbare g/w/b op bepaalde variabele",
          shinycssloaders::withSpinner(leafletOutput("map_variable"))) 
    })
    
    #Creates box for staafdiagram only when one of the subthemes is selected that has one
    output[["box_staafdiagram"]] <- renderUI({
      
      #subthemes that have data available on the count inside a radius
      subthemes_count <- c("Huisartsenpraktijk","Ziekenhuis incl. buitenpolikliniek","Ziekenhuis excl. buitenpolikliniek", "Supermarkt", "Overige dagelijkse levensmiddelen", "Warenhuis",
                           "Cafe", "Cafetaria", "Restaurant", "Hotel", "Kinderdagverblijf", "Buitenschoolse opvang", "Basisschool",
                           "Voortgezet onderwijs", "VMBO school", "HAVO/VWO school", "Bioscoop", "Attractie", "Podiumkunsten", "Museum")
      
      title <- paste0("Staafdiagram: ",selected_subtheme_title())
      
      #if the selected subthemes is in these subthemes with count, show the "staafdiagram" box
      if(input$subthema %in% subthemes_count){
        box(title = title, width = 4, status = "warning", solidHeader = T,
            "Aantal van het gekozen subthema binnen een bepaalde afstand, voor het gekozen gebied (roze) en andere vergelijkbare gebieden (blauw).",
            #"Hier komt een staafdiagram om je wijk te vergelijken met het gemiddelde van vergelijkbare wijken",
            plotOutput("plot_variable"))
      }
    })
                         
    # ONGEVALLEN/TRAFFIC INCIDENTS
    # Make selection dependent on previous input
    observeEvent(input$gemeente22, {
      updateSelectInput(session, 'wijken22',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente22]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$gemeente23, {
      updateSelectInput(session, 'wijken23',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente23]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$wijken23,{
      updateSelectInput(session, 'buurten23',
                        choices = unique(postcodes_final$buurtnaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente23 & 
                                                                         postcodes_final$wijknaam2020==input$wijken23]))       # Only display buurten that are in the selected wijk
    })
    
    # REACTION ON ACTION 2
    # Create input dataset for further analysis
    reaction2 <- eventReactive(input$action2, {
      sf_use_s2(F)
      polygons_niveau <- all_polygons %>% filter(Niveau == input$niveau2)  # Only polygons on selected niveau (gemeente, wijk, buurt)
      intersection <- intersection %>% filter(Niveau == input$niveau2 & 
                                                JAAR_VKL == input$jaar)    # Precalculated intersection filtered on niveau and year of input
      
      df_intersection <- as.data.frame(intersection) 
      df_polygons <- as.data.frame(all_polygons)
      
      if (input$niveau2 == "Gemeenten"){
        pol_select <- polygons_niveau %>% filter(GM_NAAM == input$gemeente21)
        incidents_count_niveau <- df_intersection %>% 
          group_by(GM_NAAM) %>% 
          count()
        number_incidents <- incidents_count_niveau %>% filter(GM_NAAM == input$gemeente21)
        intersection_select <- intersection %>% filter(GM_NAAM == input$gemeente21 &
                                                         Niveau == "Gemeenten")
        stedelijkheid_num <- df_polygons %>% filter(GM_NAAM == input$gemeente21 &
                                                      Niveau == "Gemeenten") %>% select(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)

      } else if(input$niveau2 == "Wijken"){
        pol_select <- polygons_niveau %>% filter(GM_NAAM == input$gemeente22 & 
                                                   WK_NAAM == input$wijken22)
        incidents_count_niveau <- df_intersection %>% 
          group_by(WK_NAAM, GM_NAAM) %>% 
          count()
        number_incidents <- incidents_count_niveau %>% filter(GM_NAAM == input$gemeente22 & 
                                                                WK_NAAM == input$wijken22)
        intersection_select <- intersection %>% filter(GM_NAAM == input$gemeente22 &
                                                         WK_NAAM == input$wijken22 &
                                                         Niveau == "Wijken")
        stedelijkheid_num <- df_polygons %>% filter(GM_NAAM == input$gemeente22 &
                                                      WK_NAAM == input$wijken22 &
                                                      Niveau == "Wijken") %>% select(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)

      } else if (input$niveau2 == "Buurten"){
        pol_select <- polygons_niveau %>% filter(GM_NAAM == input$gemeente23 & 
                                                   WK_NAAM == input$wijken23 & 
                                                   BU_NAAM == input$buurten23)
        incidents_count_niveau <- df_intersection %>% 
          group_by(BU_NAAM, WK_NAAM, GM_NAAM) %>% 
          count()
        number_incidents <- incidents_count_niveau %>% filter(GM_NAAM == input$gemeente23 &
                                                                WK_NAAM == input$wijken23 &
                                                                BU_NAAM == input$buurten23)   
        intersection_select <- intersection %>% filter(GM_NAAM == input$gemeente23 &
                                                         WK_NAAM == input$wijken23 &
                                                         BU_NAAM == input$buurten23 &
                                                         Niveau == "Buurten")
        stedelijkheid_num <- df_polygons %>% filter(GM_NAAM == input$gemeente23 &
                                                      WK_NAAM == input$wijken23 &
                                                      BU_NAAM == input$buurten23 &
                                                      Niveau == "Buurten") %>% select(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
      }
      
      number_incidents <- number_incidents$n
      number_incidents <- ifelse(is.na(number_incidents), "0", number_incidents)
      
      stedelijkheid_num <- stedelijkheid_num$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`
      
      list_ongevallen_return <- list("pol_select" = pol_select,                    # Dataset with selected polygon/area 
                                     "intersection_select" = intersection_select,  # All point data of selected area in selected year
                                     "number_incidents" = number_incidents,        # Number of incidents in selected area and year 
                                     "stedelijkheid_num" = stedelijkheid_num       # Stedelijkheid number for valuebox
                                     )               
                                      
      return(list_ongevallen_return)   
    })
    
    output$stedelijkheid_num <- renderValueBox(
      valueBox(reaction2()$stedelijkheid_num, HTML("Het stedelijkheidsniveau van het door u geselecteerde gebied. <br>
               1 = zeer stedelijk, 5 = niet stedelijk"),
               icon = icon("city", class = "fa-solid fa-city", lib = "font-awesome"),
               color = "red") # Valuebox
    ) # Render valuebox
    
    # General trend in selected area
    input_trend <- eventReactive(input$action2, {
      if (input$niveau2 == "Gemeenten"){
        area <- intersection %>% filter(GM_NAAM == input$gemeente21 & 
                                          Niveau == "Gemeenten")
      } else if(input$niveau2 == "Wijken"){
        area <- intersection %>% filter(GM_NAAM == input$gemeente22 & 
                                          WK_NAAM == input$wijken22 & 
                                          Niveau == "Wijken")
      } else if (input$niveau2 == "Buurten"){
        area <- intersection %>% filter(GM_NAAM == input$gemeente23 & 
                                          WK_NAAM == input$wijken23 & 
                                          BU_NAAM == input$buurten23 & 
                                          Niveau == "Buurten")
      }
      return(area)
    })
    
    # Trend line of accidents in selected area over the years
    output$general_trend <- renderPlot({
      input_trend() %>% count(JAAR_VKL) %>%
        ggplot() +
        geom_line(aes(x = as.factor(JAAR_VKL), y = n, group = 1), color = "Red") +
        labs(title = "Aantal ongevallen per jaar",
             x = "Jaar",
             y = "Aantal") +
        theme_classic() +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 14))
    })
    
    # Reaction on subthema input with the correct dataset
    reaction3 <- eventReactive(input$action2, {
      intersection_select <- reaction2()$intersection_select                                        # Points in selected polygon (filtered on niveau, naam and year)
      if (input$subthema2 == "WGD_CODE_1"){
        color_incidents <- colorFactor(brewer.pal(n = 6, "Set3"), intersection_select$WGD_CODE_1)   # Create color palette for categoric variable
        subthema <- intersection_select$WGD_CODE_1                                                  # Set subthema that can be used for color map
        subthema_char <-"Weersgesteldheid"                                                          # Character of subthema for title
        
        bar_chart <- intersection_select %>%
          count(WGD_CODE_1) %>% 
          ggplot(aes(x = WGD_CODE_1, y=n, fill = as.factor(WGD_CODE_1))) +
          geom_col(show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          scale_fill_brewer(palette = "Set3") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 14)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          geom_text(aes(label=n), nudge_y = .4, size = 6) 
        
        pie_chart <- intersection_select %>%
          count(WGD_CODE_1) %>% 
          ggplot(aes(x = "", y = n, fill = as.factor(WGD_CODE_1))) +
          geom_bar(stat = "identity", color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          scale_fill_brewer(palette = "Set3") +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char) +
          theme(legend.text = element_text(size = 16))+
          geom_text_repel(aes(x = 1.6, 
                        label = paste0(n,
                                       " (",
                                       scales::percent(n / sum(n)),
                                       ")")),
                    position = position_stack(vjust = 0.5))
      } else if (input$subthema2 == "AP3_OMS"){
        color_incidents <- colorFactor(brewer.pal(n = 3, "Set3"), intersection_select$AP3_OMS)
        subthema <- intersection_select$AP3_OMS 
        subthema_char <-"Afloop"
        
        bar_chart <- intersection_select %>% 
          count(AP3_OMS) %>% 
          ggplot(aes(x = AP3_OMS, y=n, fill = as.factor(AP3_OMS))) +
          geom_col(show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          scale_fill_brewer(palette = "Set3") +
          theme_classic()+
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 14)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          geom_text(aes(label=n), nudge_y = .4, size = 6) 
        
        pie_chart <- intersection_select %>%
          count(AP3_OMS) %>% 
          ggplot(aes(x = "", y = n, fill = as.factor(AP3_OMS))) +
          geom_bar(stat = "identity", color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          scale_fill_brewer(palette = "Set3") +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char) +
          theme(legend.text = element_text(size = 16))+
          geom_text_repel(aes(x = 1.6, 
                        label = paste0(n,
                                       " (",
                                       scales::percent(n / sum(n)),
                                       ")")),
                    position = position_stack(vjust = 0.5))
      } else if (input$subthema2 == "AOL_OMS"){
        color_incidents <- colorFactor(brewer.pal(n = 10, "Set3"), intersection_select$AOL_OMS)
        subthema <- intersection_select$AOL_OMS 
        subthema_char <-"Aard"
        
        bar_chart <- intersection_select %>% 
          count(AOL_OMS) %>% 
          ggplot(aes(x = AOL_OMS, y=n, fill = as.factor(AOL_OMS))) +
          geom_col(show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          scale_fill_brewer(palette = "Set3") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 14)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          geom_text(aes(label=n), nudge_y = .4, size = 6) 
        
        pie_chart <- intersection_select %>%
          count(AOL_OMS) %>% 
          ggplot(aes(x = "", y = n, fill = as.factor(AOL_OMS))) +
          geom_bar(stat = "identity", color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          scale_fill_brewer(palette = "Set3") +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char) +
          theme(legend.text = element_text(size = 16))+
          geom_text_repel(aes(x = 1.6, 
                        label = paste0(n,
                                       " (",
                                       scales::percent(n / sum(n)),
                                       ")")),
                    position = position_stack(vjust = 0.5))
      } else if (input$subthema2 == "OTE_OMS"){
        color_incidents <- colorFactor(brewer.pal(n = 10, "Set3"), intersection_select$OTE_OMS)
        subthema <- intersection_select$OTE_OMS 
        subthema_char <-"Objecttype"
        
        bar_chart <- intersection_select %>% 
          count(OTE_OMS) %>% 
          ggplot(aes(x = OTE_OMS, y=n, fill = as.factor(OTE_OMS))) +
          geom_col(show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          scale_fill_brewer(palette = "Set3") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 14)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          geom_text(aes(label=n), nudge_y = .4, size = 6) 
        
        pie_chart <- intersection_select %>%
          count(OTE_OMS) %>% 
          ggplot(aes(x = "", y = n, fill = as.factor(OTE_OMS))) +
          geom_bar(stat = "identity", color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          scale_fill_brewer(palette = "Set3") +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char) +
          theme(legend.text = element_text(size = 16))+
          geom_text_repel(aes(x = 1.6, 
                        label = paste0(n,
                                       " (",
                                       scales::percent(n / sum(n)),
                                       ")")),
                    position = position_stack(vjust = 0.5))
      } else if (input$subthema2 == "WSE_OMS"){
        color_incidents <- colorFactor(brewer.pal(n = 9, "Set3"), intersection_select$WSE_OMS)
        subthema <- intersection_select$WSE_OMS 
        subthema_char <-"Wegsituatie"
        
        bar_chart <- intersection_select %>% 
          count(WSE_OMS) %>% 
          ggplot(aes(x = WSE_OMS, y=n, fill = as.factor(WSE_OMS))) +
          geom_col(show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          scale_fill_brewer(palette = "Set3") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 14)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          geom_text(aes(label=n), nudge_y = .4, size = 6) 
        
        pie_chart <- intersection_select %>%
          count(WSE_OMS) %>% 
          ggplot(aes(x = "", y = n, fill = as.factor(WSE_OMS))) +
          geom_bar(stat = "identity", color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          scale_fill_brewer(palette = "Set3") +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char) +
          theme(legend.text = element_text(size = 16))+
          geom_text_repel(aes(x = 1.6,
                        label = paste0(n,
                                       " (",
                                       scales::percent(n / sum(n)),
                                       ")")),
                    position = position_stack(vjust = 0.5))
      } else if (input$subthema2 == "MAXSNELHD"){
        color_incidents <- colorFactor(brewer.pal(n = 11, "Blues"), intersection_select$MAXSNELHD)
        subthema <- intersection_select$MAXSNELHD 
        subthema_char <-"Maximum snelheid"
        
        bar_chart <- intersection_select %>% 
          count(MAXSNELHD) %>% 
          ggplot(aes(x = as.factor(MAXSNELHD), y=n, fill = as.factor(MAXSNELHD))) +
          geom_col(show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          scale_fill_brewer(palette = "Blues") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 14)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          geom_text(aes(label=n), nudge_y = .4, size = 6) 
        
        pie_chart <- intersection_select %>%
          count(MAXSNELHD) %>% 
          ggplot(aes(x = "", y = n, fill = as.factor(MAXSNELHD))) +
          geom_bar(stat = "identity", color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          scale_fill_brewer(palette = "Blues") +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char) +
          theme(legend.text = element_text(size = 16))+
          geom_text_repel(aes(x = 1.6,
                        label = paste0(n,
                                       " (",
                                       scales::percent(n / sum(n)),
                                       ")")),
                    position = position_stack(vjust = 0.5))
      } else if (input$subthema2 == "ANTL_PTJ"){
        colorCount <- length(unique(intersection_select$ANTL_PTJ))
        color_incidents <- col_factor("Blues", NULL)
        subthema <- intersection_select$ANTL_PTJ 
        subthema_char <-"Aantal partijen"
        
        bar_chart <- intersection_select %>% 
          count(ANTL_PTJ) %>% 
          ggplot(aes(x = as.factor(ANTL_PTJ), y=n, fill = as.factor(ANTL_PTJ))) +
          geom_col(show.legend = F) +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               x = subthema_char,
               y = "Aantal") + 
          theme_classic()+
          scale_fill_brewer(palette = "Blues") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 14)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          geom_text(aes(label=n), nudge_y = .4, size = 6) 
        
        pie_chart <- intersection_select %>%
          count(ANTL_PTJ) %>% 
          ggplot(aes(x = "", y = n, fill = as.factor(ANTL_PTJ))) +
          geom_bar(stat = "identity", color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          scale_fill_brewer(palette = "Blues") +
          labs(title = "Aantal ongevallen binnen geselecteerd subthema",
               fill = subthema_char) +
          theme(legend.text = element_text(size = 16))+
          geom_text_repel(aes(x = 1.6,
                        label = paste0(n,
                                       " (",
                                       scales::percent(n / sum(n)),
                                       ")")),
                    position = position_stack(vjust = 0.5))
      } 
      

      list_return <- list("color_incidents" = color_incidents,         # Color palette for categorical variable
                          "subthema" = subthema,                       # Name of subthema variable for the function 
                          "intersection_select" = intersection_select, # All points of incidents in selected area for selected year 
                          "subthema_char" = subthema_char,             # Character: title of legend
                          "pie_chart" = pie_chart,                     # Pie chart with ggplot reactive on subthema
                          "bar_chart" = bar_chart                      # Bar chart with ggplot reactive on subthema
                          )                     
      
      return(list_return)
    }) # Function reaction 3
    
    # New map with coloring of selected variable
    color_map_incidents <- function(df,                     # Dataset of points in selected area and year
                                    subthema,               # Subthema selected in input
                                    subthema_char,          # Name for subthema in legend title
                                    color_incidents){       # Color palette
      map <- leaflet(df) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(radius = 5,
                         color = ~color_incidents(subthema),
                         label = ~subthema) %>% 
        addPolylines(data = reaction2()$pol_select,
                     stroke = T,
                     weight = 3) %>%
        addLegend(pal = color_incidents, 
                  values = ~subthema,
                  title = subthema_char)
      
      return(map)
    } # Color_map_incidents function

    # Map output of the incidents colored by the selected variable
    output$map_color_incidents <- renderLeaflet({
      color_map_incidents(reaction3()$intersection_select,
                          reaction3()$subthema,
                          reaction3()$subthema_char,
                          reaction3()$color_incidents)
    })
    
    # Bar chart 
    output$bar_chart <- renderPlot({
      reaction3()$bar_chart
    })
    
    # Pie chart
    output$pie_chart <- renderPlot({
      reaction3()$pie_chart
    })
    
    # Create function for number of incidents in selected area
    top_incidents <- eventReactive(input$action2, {
      comparable_df <- as.data.frame(intersection) %>% 
        filter(Niveau == input$niveau2 & 
                 JAAR_VKL == input$jaar &
                 `Stedelijkheid..1.zeer.sterk.stedelijk..5.niet.stedelijk.` == reaction2()$stedelijkheid_num) 
      if (input$niveau2 == "Gemeenten"){
        incidents_count_niveau <- comparable_df %>% 
          group_by(GM_NAAM, .drop = FALSE) %>% 
          count()
        names_all_niveau <- as.data.frame(all_polygons) %>% 
          filter(Niveau == "Gemeenten" &
                   `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)` == reaction2()$stedelijkheid_num)
        no_incidents_niveau <- anti_join(names_all_niveau, incidents_count_niveau, "GM_NAAM") %>% 
          select(GM_NAAM) %>% 
          mutate(n = 0)
        incidents_all_inclu_niveau <- rbind(incidents_count_niveau, no_incidents_niveau)
        sorted <- incidents_count_niveau %>% arrange(desc(n))           # In descending order, so most incidents are at top
        sorted$Rank <- seq.int(nrow(sorted))                            # Add row numbers to dataframe as indication of place in order
        sorted <- sorted %>% relocate(Rank)                             # Rank as first column 
        pol_selected <- sorted %>% filter(GM_NAAM == input$gemeente21)  # Selected polygon and number of incidents
        top5_incidents <- head(sorted, 5)
        top5_incidents <- rbind(top5_incidents, pol_selected)
        top5_incidents <- top5_incidents %>% distinct(GM_NAAM, .keep_all = TRUE)
        top5_incidents <- rename(top5_incidents, "Gemeente" = GM_NAAM,
                                 "Aantal" = n)
        
        number_incidents <- incidents_count_niveau %>% 
          filter(GM_NAAM == input$gemeente21)
        
      } else if(input$niveau2 == "Wijken"){
        incidents_count_niveau <- comparable_df %>% 
          group_by(WK_NAAM, GM_NAAM, .drop = FALSE) %>% 
          count()
        names_all_niveau <- as.data.frame(all_polygons) %>% 
          filter(Niveau == "Wijken" &
                   `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)` == reaction2()$stedelijkheid_num)
        no_incidents_niveau <- anti_join(names_all_niveau, incidents_count_niveau, by = c("WK_NAAM", "GM_NAAM")) %>% 
          select(WK_NAAM, GM_NAAM) %>% 
          mutate(n = 0)
        incidents_all_inclu_niveau <- rbind(incidents_count_niveau, no_incidents_niveau)
        sorted <- incidents_all_inclu_niveau %>% arrange(desc(n)) 
        sorted$Rank <- seq.int(nrow(sorted))                   
        sorted <- sorted %>% relocate(Rank)
        pol_selected <- sorted %>% filter(GM_NAAM == input$gemeente22 & 
                                            WK_NAAM == input$wijken22)
        top5_incidents <- head(sorted, 5)
        top5_incidents <- rbind(top5_incidents, pol_selected)
        top5_incidents <- top5_incidents %>% distinct(GM_NAAM, WK_NAAM, .keep_all = TRUE)
        top5_incidents <- rename(top5_incidents, "Gemeente" = GM_NAAM,
                                "Wijk" = WK_NAAM,
                                 "Aantal" = n)
        number_incidents <- incidents_all_inclu_niveau %>% 
          filter(GM_NAAM == input$gemeente22 & 
                   WK_NAAM == input$wijken22)
      } else if(input$niveau2 == "Buurten"){
        incidents_count_niveau <- comparable_df %>% 
          group_by(BU_NAAM, WK_NAAM, GM_NAAM, .drop = FALSE) %>% 
          count()
        
        names_all_niveau <- as.data.frame(all_polygons) %>% 
          filter(Niveau == "Buurten" &
                   `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)` == reaction2()$stedelijkheid_num)
        no_incidents_niveau <- anti_join(names_all_niveau, incidents_count_niveau, by = c("BU_NAAM", "WK_NAAM", "GM_NAAM")) %>% 
          select(BU_NAAM, WK_NAAM, GM_NAAM) %>% 
          mutate(n = 0)
        incidents_all_inclu_niveau <- rbind(incidents_count_niveau, no_incidents_niveau)
        sorted <- incidents_all_inclu_niveau %>% arrange(desc(n))
        sorted$Rank <- seq.int(nrow(sorted))                   
        sorted <- sorted %>% relocate(Rank) 
        pol_selected <- sorted %>% filter(GM_NAAM == input$gemeente23 & 
                                            WK_NAAM == input$wijken23 & 
                                            BU_NAAM == input$buurten23)    
        top5_incidents <- head(sorted, 5)
        top5_incidents <- rbind(top5_incidents, pol_selected)
        top5_incidents <- top5_incidents %>% distinct(GM_NAAM, WK_NAAM, BU_NAAM, .keep_all = TRUE)
        
        top5_incidents <- rename(top5_incidents, "Gemeente" = GM_NAAM,
                                 "Wijk" = WK_NAAM,
                                 "Buurt" = BU_NAAM,
                                 "Aantal" = n)
        number_incidents <- incidents_all_inclu_niveau %>% 
          filter(GM_NAAM == input$gemeente23 & 
                   WK_NAAM == input$wijken23 & 
                   BU_NAAM == input$buurten23)
        
      }
      
      number_incidents$n <- round(number_incidents$n)
      count_incidents_niveau <- as.integer(number_incidents$n)
      incidents_mean <- round(mean(incidents_all_inclu_niveau$n))
      
      list_return <- list("top5_incidents" = top5_incidents,                          # Top 5 most incidents in dataframe with name and count
                          "incidents_all_inclu_niveau" = incidents_all_inclu_niveau,  # Dataframe with all names and also count 0
                          "count_incidents_niveau" = count_incidents_niveau,          # Number of incidents 
                          "incidents_mean" = incidents_mean                           # Mean number of incidents of comparable areas
                          )

      return(list_return)  # Dataframe with top 6 areas with the most incidents + selected polygon
      
    })  # Event reactive
    
    # Output number of incidents in selected area
    output$count_incidents <- renderTable(
      top_incidents()$top5_incidents,
      rownames = F,
    ) # Render table
    
    # Only if incidents happened, plot second row for subtheme
    output[["subtheme_row"]] <- renderUI({
      if (top_incidents()$count_incidents_niveau > 0){
        fluidRow(
          column(width = 6,
                 box(title = "Kaart met incidenten", width = NULL, status = "warning", solidHeader = T,
                     HTML("Hier wordt de kaart weergegeven met punten van incidenten. De kleuren komen overeen met de categorieën van de geselecteerde variabele.<br>
                                            U kunt de kaart vergroten en verkleinen door te scrollen. Door met uw muis over de punten te bewegen kunt u de categorie zien."),
                     shinycssloaders::withSpinner(leafletOutput("map_color_incidents"))) # Box incidenten en kleur
          ), # Column kaart
          column(width = 6,
                 tabBox(width = NULL, id="tabset1",
                        tabPanel("Staafdiagram", "Dit staafdiagram geeft het aantal ongelukken in een bepaalde categorie weer.", 
                                 plotOutput("bar_chart")),
                        tabPanel("Taartdiagram", "Dit taartdiagram geeft de verhouding van het aantal ongelukken in een bepaalde categorie weer", 
                                 plotOutput("pie_chart"))
                 ) # Tabbox diagram
          ) # Column diagram variabele
        ) # Fluid row grafieken thema
      } else if (top_incidents()$count_incidents_niveau <= 0){
        h4("Omdat er geen incidenten hebben plaatsgevonden, kan geen informatie worden gegeven over het geselecteerde subthema.")
      }
    }) # Render UI subtheme row

    
    # Output number of incidents in selected area
    output$number_incidents <- renderValueBox(
      valueBox(as.character(top_incidents()$count_incidents_niveau), "ongevallen in het door u geselecteerde gebied", 
               icon = icon("car-crash", class = "fa-solid fa-car-burst", lib = "font-awesome"), 
               color = "red") # valuebox
    ) # rendervaluebox
    
    # Create histogram for comparability 
    output$histogram_incidents <- renderPlot(
      ggplot(data = top_incidents()$incidents_all_inclu_niveau, aes(x = n)) +
        geom_histogram(binwidth = 4, fill = "dark gray") +
        geom_vline(xintercept = top_incidents()$count_incidents_niveau, color = "blue", size = 1.5) +
        geom_vline(xintercept = top_incidents()$incidents_mean, color = "green4", size = 1.5) +
        annotate(x = top_incidents()$count_incidents_niveau, y = +Inf, 
                 geom = "label", label = "Geselecteerd gebied", vjust = 2, color = "blue")+
        annotate(x = top_incidents()$incidents_mean, y = +Inf, 
                 geom = "label", label = "Gemiddelde", vjust = 4, color = "green4") +
        annotate(x = top_incidents()$incidents_mean, y = +Inf, 
                 label = as.character(top_incidents()$incidents_mean), vjust = 5, 
                 geom = "label", color = "green4") +
        labs(title = "Verdeling van aantal ongelukken in vergelijkbare gebieden",
            x = "Aantal ongelukken",
            y = "Aantal gebieden") +
        theme(axis.text = element_text(size = 18),
              axis.title = element_text(size = 18)) +
        theme_classic()
    )                 
    
    
    ######START GEZONDHEID
    
    
    
    # Make selection dependent on previous input
    observeEvent(input$gemeente2_gez, {
      updateSelectInput(session, 'wijken2_gez',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente2_gez]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$gemeente3_gez, {
      updateSelectInput(session, 'wijken3_gez',
                        choices = unique(postcodes_final$wijknaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente3_gez]))  # Only display that are in the selected gemeente
    })
    observeEvent(input$wijken3_gez,{
      updateSelectInput(session, 'buurten3_gez',
                        choices = unique(postcodes_final$buurtnaam2020[postcodes_final$Gemeentenaam2020 == input$gemeente3_gez & 
                                                                         postcodes_final$wijknaam2020==input$wijken3_gez]))       # Only display buurten that are in the selected wijk
    })
    
    observeEvent(input$thema_gez, {
      if (input$thema_gez == "Gezondheid en beperkingen") {
        updateSelectInput(session, 'subthema_gez', 
                          choices = c("Zeer goede of goede gezondheid (%)", "Langdurige aandoening (%)", 
                                      "Langdurige ziekte en beperkt (%)", "Beperking", "Beperkt vanwege gezondheid",
                                      "Risico op angststoornis of depressie", "(heel) veel stress (%)"))
      }else if (input$thema_gez == "Leefstijl") {
        updateSelectInput(session, 'subthema_gez', 
                          choices = c("Voldoet aan beweegrichtlijn (%)", "Wekelijkse sporters (%)", "Gewicht",
                                      "Rokers (%)", "Alcoholgebruik", "Lopen/fietsen naar school of werk"))
      }else if (input$thema_gez == "Participatie en omgeving") {
        updateSelectInput(session, 'subthema_gez', 
                          choices = c("Matig tot veel regie over eigen leven (%)", "Eenzaamheid", "Mantelzorger (%)", "Vrijwilligerswerk (%)", 
                                      "Ernstige geluidhinder door buren (%)", "Moeite met rondkomen (%)"))
      }
    })
    
    #Change input choices for histogram depending on subtheme
    observeEvent(input$action_thema_gez, {
      if (input$subthema_gez == "Beperking") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Lichamelijke beperking (%)", "Beperking in horen (%)", "Beperking in zien (%)","Beperking in bewegen (%)"))
      }else if(input$subthema_gez == "Beperkt vanwege gezondheid") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Beperkt vanwege gezondheid (%)","Ernstig beperkt vanwege gezondheid (%)"))
      }else if(input$subthema_gez == "Risico op angststoornis of depressie") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Matig tot hoog risico op angststoornis of depressie (%)","Hoog risico op angststoornis of depressie (%)"))
      }else if(input$subthema_gez == "Gewicht") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Ondergewicht (%)","Normaal gewicht (%)","Overgewicht (%)","Ernstig overgewicht (%)"))
      }else if(input$subthema_gez == "Alcoholgebruik") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Voldoet aan alcoholrichtlijn (%)","Drinkers (%)","Zware drinkers (%)","Overmatige drinkers (%)"))
      }else if(input$subthema_gez == "Lopen/fietsen naar school of werk") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)"))
      }else if(input$subthema_gez == "Eenzaamheid") {
        updateSelectInput(session, 'categorie_hist', 
                          choices = c("Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)"))
      }
    })
    
    #Change input choices for line plot depending on subtheme
    observeEvent(input$action_thema_gez, {
      if (input$subthema_gez == "Beperking") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Lichamelijke beperking (%)", "Beperking in horen (%)", "Beperking in zien (%)","Beperking in bewegen (%)"))
      }else if(input$subthema_gez == "Beperkt vanwege gezondheid") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Beperkt vanwege gezondheid (%)","Ernstig beperkt vanwege gezondheid (%)"))
      }else if(input$subthema_gez == "Risico op angststoornis of depressie") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Matig tot hoog risico op angststoornis of depressie (%)","Hoog risico op angststoornis of depressie (%)"))
      }else if(input$subthema_gez == "Gewicht") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Ondergewicht (%)","Normaal gewicht (%)","Overgewicht (%)","Ernstig overgewicht (%)"))
      }else if(input$subthema_gez == "Alcoholgebruik") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Voldoet aan alcoholrichtlijn (%)","Drinkers (%)","Zware drinkers (%)","Overmatige drinkers (%)"))
      }else if(input$subthema_gez == "Lopen/fietsen naar school of werk") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)"))
      }else if(input$subthema_gez == "Eenzaamheid") {
        updateSelectInput(session, 'categorie_line', 
                          choices = c("Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)"))
      }
    })
    
    #Change input choices for staafdiagram depending on subtheme
    observeEvent(input$action_thema_gez, {
      if (input$subthema_gez == "Beperking") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Lichamelijke beperking (%)", "Beperking in horen (%)", "Beperking in zien (%)","Beperking in bewegen (%)"))
      }else if(input$subthema_gez == "Beperkt vanwege gezondheid") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Beperkt vanwege gezondheid (%)","Ernstig beperkt vanwege gezondheid (%)"))
      }else if(input$subthema_gez == "Risico op angststoornis of depressie") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Matig tot hoog risico op angststoornis of depressie (%)","Hoog risico op angststoornis of depressie (%)"))
      }else if(input$subthema_gez == "Gewicht") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Ondergewicht (%)","Normaal gewicht (%)","Overgewicht (%)","Ernstig overgewicht (%)"))
      }else if(input$subthema_gez == "Alcoholgebruik") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Voldoet aan alcoholrichtlijn (%)","Drinkers (%)","Zware drinkers (%)","Overmatige drinkers (%)"))
      }else if(input$subthema_gez == "Lopen/fietsen naar school of werk") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)"))
      }else if(input$subthema_gez == "Eenzaamheid") {
        updateSelectInput(session, 'categorie_staaf', 
                          choices = c("Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)"))
      }
    })
    
    #Change input choices for map with variable depending on subtheme
    observeEvent(input$action_thema_gez, {
      if (input$subthema_gez == "Beperking") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Lichamelijke beperking (%)", "Beperking in horen (%)", "Beperking in zien (%)","Beperking in bewegen (%)"))
      }else if(input$subthema_gez == "Beperkt vanwege gezondheid") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Beperkt vanwege gezondheid (%)","Ernstig beperkt vanwege gezondheid (%)"))
      }else if(input$subthema_gez == "Risico op angststoornis of depressie") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Matig tot hoog risico op angststoornis of depressie (%)","Hoog risico op angststoornis of depressie (%)"))
      }else if(input$subthema_gez == "Gewicht") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Ondergewicht (%)","Normaal gewicht (%)","Overgewicht (%)","Ernstig overgewicht (%)"))
      }else if(input$subthema_gez == "Alcoholgebruik") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Voldoet aan alcoholrichtlijn (%)","Drinkers (%)","Zware drinkers (%)","Overmatige drinkers (%)"))
      }else if(input$subthema_gez == "Lopen/fietsen naar school of werk") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Lopen en of fietsen naar school of werk (%)","Lopen naar school of werk (%)","Fietsen naar school of werk (%)"))
      }else if(input$subthema_gez == "Eenzaamheid") {
        updateSelectInput(session, 'categorie_map', 
                          choices = c("Eenzaam (%)","Ernstig eenzaam (%)","Emotioneel eenzaam (%)","Sociaal eenzaam (%)"))
      }
    })
    
    #Dividing the subthemes about health between normal and special (having multiple closely related variables)
    Normal <- c("Zeer goede of goede gezondheid (%)", "Langdurige aandoening (%)", 
                "Langdurige ziekte en beperkt (%)",  "(heel) veel stress (%)",
                "Voldoet aan beweegrichtlijn (%)", "Wekelijkse sporters (%)", 
                "Rokers (%)", "Matig tot veel regie over eigen leven (%)", 
                "Mantelzorger (%)", "Vrijwilligerswerk (%)", 
                "Ernstige geluidhinder door buren (%)","Moeite met rondkomen (%)")
    Special <- c("Beperking", "Gewicht", "Alcoholgebruik", "Lopen/fietsen naar school of werk", "Eenzaamheid", 
                 "Risico op angststoornis of depressie", "Beperkt vanwege gezondheid")

    
    #make used GEZONDHEID data reactive on the selected niveau
    Gez_datasetInput <- eventReactive(input$action_gez,{
      df <- as.data.frame(gezondheid_all)
      df <- df[df$Niveau == input$niveau_gez,]
      if(input$niveau_gez == 'Gemeenten'){
        if(input$vergelijkbaar1_gez == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
          stedelijkheid_num <- stedelijkheid_num[1]
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
        }else if (input$vergelijkbaar1_gez == "Inkomensniveau"){
          inkomen_num <- df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(`inkomengroep`)         
          inkomen_num<-inkomen_num[1]
          comparable_df <- df[df$inkomengroep == inkomen_num, ]
        }else if(input$vergelijkbaar1_gez == "Nederland"){
          comparable_df <- df
        }else if(input$vergelijkbaar1_gez == "age_distribution"){
          comparable_df <- df
        }
        selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(CODE)
        comparable_df$selected_area_code <- selected_area_code[1]
        if(input$vergelijkbaar1_gez=="age_distribution"){
          comparable_df <- similar_age(comparable_df, input$niveau_gez)
          selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(CODE)
          comparable_df$selected_area_code <- selected_area_code[1]
        }
        selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente1_gez) %>% pull(NAAM)
        comparable_df$selected_area_label <- selected_area_label[1]
        selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente1)
        selected_polygon <- selected_polygon[1,]
        row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente1_gez)
        row_num_selected <-row_num_selected[1]
      }else if(input$niveau_gez == 'Wijken'){
        if(input$vergelijkbaar2_gez == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df %>% filter(WK_NAAM == input$wijken2_gez & GM_NAAM == input$gemeente2_gez) %>% pull(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
          stedelijkheid_num <- stedelijkheid_num[1]
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
          comparable_df <- comparable_df %>% drop_na(CODE)
          output$error_vergelijkbaarheid_gez <- renderText(
            print("")
          )
        }else if (input$vergelijkbaar2_gez == "Inkomensniveau"){
          inkomen_num <-df %>% filter(WK_NAAM == input$wijken2_gez & GM_NAAM == input$gemeente2_gez) %>% pull(`inkomengroep`)
          inkomen_num<-inkomen_num[1]
          if(is.na(inkomen_num)){
            comparable_df <- df[df$Niveau == input$niveau_gez,]
            output$error_vergelijkbaarheid_gez <- renderText(
              print("Let op, door een missende waarde van het inkomensniveau voor uw wijk, wordt er nu met heel Nederland vergeleken.")
            )
          }else{
            comparable_df <- df[df$inkomengroep == inkomen_num, ]
            comparable_df <- comparable_df %>% drop_na(CODE)
            output$error_vergelijkbaarheid_gez <- renderText(
              print("")
            )
          }
        }else if(input$vergelijkbaar2_gez == "Nederland"){
          comparable_df <- df
          output$error_vergelijkbaarheid_gez <- renderText(
            print("")
          )
        }else if(input$vergelijkbaar2_gez == "age_distribution"){
          comparable_df <- df
        }
        selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente2_gez & WK_NAAM == input$wijken2_gez) %>%pull(CODE)
        comparable_df$selected_area_code <- selected_area_code[1]
        if(input$vergelijkbaar2_gez == "age_distribution"){
          comparable_df <- similar_age(comparable_df, input$niveau_gez)
          if(any(is.na(comparable_df$`Personen 0 tot 15 jaar (%)`))){
            comparable_df <- df 
            output$error_vergelijkbaarheid_gez <- renderText(
              print("Let op, door missende gegevens over de leeftijdsopbouw voor uw wijk, wordt er nu met heel Nederland vergeleken.")
            )
          }else{
            comparable_df <- comparable_df
            output$error_vergelijkbaarheid_gez <- renderText(
              print("")
            )
          }
          selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente2_gez & WK_NAAM == input$wijken2_gez) %>%pull(CODE)
          comparable_df$selected_area_code <- selected_area_code[1]
        }
        selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente2_gez & WK_NAAM == input$wijken2_gez) %>%pull(NAAM)
        comparable_df$selected_area_label <- selected_area_label[1]
        selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente2_gez & WK_NAAM == input$wijken2_gez)
        selected_polygon <- selected_polygon[1,]
        row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente2_gez & comparable_df$WK_NAAM == input$wijken2_gez)
        row_num_selected <-row_num_selected[1]
      }else if(input$niveau_gez == 'Buurten'){
        if(input$vergelijkbaar3_gez == "Stedelijkheidsniveau"){
          stedelijkheid_num <- df%>% filter(BU_NAAM==input$buurten3_gez & GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez) %>% pull(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
          stedelijkheid_num <- stedelijkheid_num[1]
          comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
          comparable_df <- comparable_df %>% drop_na(CODE)
          output$error_vergelijkbaarheid_gez <- renderText(
            print("")
          )
        } else if(input$vergelijkbaar3_gez == "Nederland"){
          comparable_df <- df
          output$error_vergelijkbaarheid_gez <- renderText(
            print("")
          )
        }else if(input$vergelijkbaar3_gez == "age_distribution"){
          comparable_df <- df
        }
        selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez & BU_NAAM == input$buurten3_gez) %>%pull(CODE)
        comparable_df$selected_area_code <- selected_area_code[1]
        if(input$vergelijkbaar3_gez == "age_distribution"){
          comp_df <- similar_age(comparable_df, input$niveau_gez)
          if(any(is.na(comp_df$`Personen 0 tot 15 jaar (%)`))){
            comparable_df <- df 
            output$error_vergelijkbaarheid_gez <- renderText(
              print("Let op, door missende gegevens over de leeftijdsopbouw voor uw buurt, wordt er nu met heel Nederland vergeleken.")
            )
          }else{
            comparable_df <- comp_df
            output$error_vergelijkbaarheid_gez <- renderText(
              print("")
            )
          }
          selected_area_code <- comparable_df %>% filter(GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez & BU_NAAM == input$buurten3_gez) %>%pull(CODE)
          comparable_df$selected_area_code <- selected_area_code[1]
        }
        selected_area_label <- comparable_df %>% filter(GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez & BU_NAAM == input$buurten3_gez) %>%pull(NAAM)
        comparable_df$selected_area_label <- selected_area_label[1]
        selected_polygon <- comparable_df %>% filter(GM_NAAM == input$gemeente3_gez & WK_NAAM == input$wijken3_gez & BU_NAAM == input$buurten3_gez)
        selected_polygon <- selected_polygon[1,]
        row_num_selected <- which(comparable_df$GM_NAAM == input$gemeente3_gez & comparable_df$WK_NAAM == input$wijken3_gez & comparable_df$BU_NAAM == input$buurten3_gez)
        row_num_selected <-row_num_selected[1]
        
      }
      comparable_df$centroidxx <- comparable_df[row_num_selected, 'centroidx']
      comparable_df$centroidyy <- comparable_df[row_num_selected, 'centroidy']
      dataset <- st_as_sf(comparable_df)
      
      list_return <- list("dataset" = dataset, "selected_polygon" = selected_polygon)
      return(list_return)
    })
    
    # Get information about selected area in table
    output$info_area_gez <- renderTable({
      df <- as.data.frame(Gez_datasetInput()$dataset)
      df <- df[df$Perioden=="2020",]
      df <- df %>% drop_na(Perioden)
      selected_area_code <- df[1, "selected_area_code"]
      df <- df[df$CODE==selected_area_code,]
      df <- df %>%
        select(c("Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)", "inkomengroep"))
      df <- rename(df, "Stedelijkheid"= `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
      df[1,]
    })
    
    #Creates map to point to the selected location and comparable polygons
    #Uses function prime_map_gez from HealthPlots file
    output$prime_map2 <- renderLeaflet({
      prime_map_gez(Gez_datasetInput()$dataset)
    })
    
    #Selected subtheme
    selected_subtheme_gez <- eventReactive(input$action_thema_gez,{
      subtheme <- input$subthema_gez
      return(subtheme)
    })
    
    #creates a barplot of the selected area and the mean of comparable areas for all age classes
    #Uses function plot_gez from HealthPlots file
    output$gez_plot <- renderPlot({
      subtheme <- selected_subtheme_gez()
      plot_gez(Gez_datasetInput()$dataset, input$categorie_staaf, subtheme)
    })
    

    #Creates a line chart of the selected area and the mean of comparable areas
    #Uses function line_plot_gez from HealthPlots file
    output$gez_line_plot <- renderPlot({
      subtheme <- selected_subtheme_gez()
      line_plot_gez(Gez_datasetInput()$dataset, subtheme, input$norm_age_line, input$spec_age_line, input$categorie_line)
    })
    
    #Creates a histogram of the selected subtheme using the comparable areas
    #Uses function histogram from HealthPlots file
    output$gez_hist <- renderPlot({
      subtheme <- selected_subtheme_gez()
      histogram(Gez_datasetInput()$dataset, subtheme, input$norm_age_hist, input$categorie_hist, input$spec_age_hist)
    })
    
    #Creates a barplot per categorie if selected subtheme contains multiple variables
    #Uses function staaf_categorie from HealthPlots file
    output$staaf_cat <- renderPlot({
      subtheme <- selected_subtheme_gez()
      staaf_categorie(Gez_datasetInput()$dataset, subtheme, input$spec_age_cat)
    })
    
    #Creates a plot for the age distribution of the selected area
    #Uses function age_distribution from HealthPlots file
    output$age_distr <- renderPlot({
      age_distribution(Gez_datasetInput()$dataset)
    })
    
    #Creates map of the selected subtheme  
    #Uses function make_map_gez from HealthPlots file
    output$map_subtheme <- renderLeaflet({
      subtheme <- selected_subtheme_gez()
      make_map_gez(Gez_datasetInput()$dataset, input$age_map, subtheme, input$categorie_map)
    })
    
    #Selected area name for the box titles (changes only when 'zoeken' button is clicked)
    selected_area_title_gez <- eventReactive(input$action_gez,{
      if(input$niveau_gez=="Gemeenten"){
        title <- input$gemeente1_gez
      }else if(input$niveau_gez=="Wijken"){
        title <- input$wijken2_gez
      }else if(input$niveau_gez=="Buurten"){
        title <- input$buurten3_gez
      }
      return(title)
    })
    
    #Info box, title changes based on the selected area
    output$info_box_gez = renderUI({
      title <- paste0("Informatie over ", selected_area_title_gez())
      box(title = title, width = 3, status = "warning", solidHeader = T,
          "In de tabel hieronder ziet u wat de stedelijkheid en de inkomensgroep zijn voor het gekozen gebied.",
          tableOutput("info_area_gez"),
          "Stedelijkheid: 1 = zeer sterk stedelijk, 5 = niet stedelijk.", br(),
          "Inkomensniveau: 1 = zeer laag percentage, 4 = hoog percentage van huishoudens met een inkomen onder het sociaal minimum.",
         ) # Box informatie
    })
    
    #Age box, title changes based on the selected area
    output$age_box_gez = renderUI({
      title <- paste0("Leeftijdsopbouw ", selected_area_title_gez())
      box(title = title, width = 3, status = "warning", solidHeader = T,
          "In onderstaande staafdiagram  ziet u de leeftijdsopbouw van het gekozen gebied.",
          shinycssloaders::withSpinner(plotOutput("age_distr"))) 
      
    })
    
    #Kaart box, title changes based on the selected area
    output$kaart_box_gez = renderUI({
      title <- paste0("Kaart met ", selected_area_title_gez())
      box(title = title, width = 3, status = "warning", solidHeader = T,
          "Kaart waarop het gekozen gebied te zien is (blauwe pointer) en de gebieden waarmee wordt vergeleken.",
          shinycssloaders::withSpinner(leafletOutput("prime_map2")),
          span(textOutput("error_vergelijkbaarheid_gez"), style="color:red")) 
    })
    
    #Base which plots and where to place them on whether the chosen subtheme is normal or special
    output$plots<-renderUI({
      subtheme <- selected_subtheme_gez()
      if(subtheme %in% Normal){
        column(width =9, 
               fluidRow(
                 box(title = "Kaart", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande kaart zijn de waardes voor het gekozen subthema te zien voor het gekozen gebied (blauwe pointer)
                     en de andere gebeieden waarmee wordt vergeleken.", br(),
                     br(),
                     selectInput("age_map", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     shinycssloaders::withSpinner(leafletOutput('map_subtheme'))
                 ),
                 box(title="Histogram", width=6, status="warning", solidHeader = T,
                     "In onderstaande histogram is de frequentieverdeling voor het gekozen subthema  te zien.
                           De zwarte verticale lijn is de waarde van het gekozen gebied. Hiermee kunt u zien hoe uw gebied het doet ten opzichte van de andere gebieden.", br(),
                     br(),
                     selectInput("norm_age_hist", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     shinycssloaders::withSpinner(plotOutput("gez_hist"))),
                 
               ),
               fluidRow(
                 box(title = "Lijndiagram", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande lijndiagram is de ontwikkeling van het gekozen subthema in de tijd te zien. 
                           De roze lijn is voor het gekozen gebied en de blauwe lijn is het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).",br(),
                     br(),
                     selectInput("norm_age_line", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     shinycssloaders::withSpinner(plotOutput("gez_line_plot"))),
                 box(title = "Staafdiagram per leeftijdsklasse", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande staafdiagram is het percentage voor het gekozen subthema te zien voor de verschillende leeftijdsklasses.
                           In het roze is het gekozen gebied te zien en in het blauw het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).", br(),
                     br(),
                     shinycssloaders::withSpinner(plotOutput("gez_plot"))),
               )
        )
      }else if(subtheme %in% Special){
        column(width =9, 
               fluidRow(
                 box(title = "Staafdiagram per categorie", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande staafdiagram is het percentage voor de verschillende categorieën binnen het subthema te zien.
                      In het roze is het gekozen gebied te zien en in het blauw het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).", br(),
                     br(),
                     selectInput("spec_age_cat", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     shinycssloaders::withSpinner(plotOutput("staaf_cat"))
                 ),
                 box(title = "Kaart", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande kaart zijn de waardes voor het gekozen subthema te zien voor het gekozen gebied (blauwe pointer)
                     en de andere gebeieden waarmee wordt vergeleken.", br(),
                     br(),
                     selectInput("age_map", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     selectInput("categorie_map", "Categorie:", choices = NULL),
                     shinycssloaders::withSpinner(leafletOutput('map_subtheme'))
                 )
               ),
        
              
         fluidRow(
                 box(title="Histogram", width=6, status="warning", solidHeader = T,
                     "In onderstaande histogram is de frequentieverdeling voor de gekozen categorie  te zien. 
                           De zwarte verticale lijn is de waarde van het gekozen gebied. Hiermee kunt u zien hoe uw gebied het doet ten opzichte van de andere gebieden.", br(),
                     br(),
                     selectInput("spec_age_hist", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     selectInput("categorie_hist", "Categorie:", choices = NULL),
                     shinycssloaders::withSpinner(plotOutput("gez_hist"))
                 ),
                 box(title = "Lijndiagram", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande lijndiagram is de ontwikkeling van de gekozen categorie in de tijd te zien. 
                           De roze lijn is voor het gekozen gebied en de blauwe lijn is het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).",br(),
                     br(),
                     selectInput("spec_age_line", "Leeftijd:", c("18-65"="18-65", "65+"="65+","18+"="18+")),
                     selectInput("categorie_line", "Categorie:", choices = NULL),
                     shinycssloaders::withSpinner(plotOutput("gez_line_plot"))
                 ),
                 box(title = "Staafdiagram per leeftijdsklasse", width = 6, status = "warning", solidHeader = T,
                     "In onderstaande staafdiagram is het percentage voor de gekozen categorie te zien voor de verschillende leeftijdsklasses.
                           In het roze is het gekozen gebied te zien en in het blauw het gemiddelde van de gebieden waarmee wordt vergeleken (zie kaart hierboven).", br(),
                     br(),
                     selectInput("categorie_staaf", "Categorie:", choices = NULL),
                     shinycssloaders::withSpinner(plotOutput("gez_plot"))
                 ),
               )
        )
      }
    })

})

