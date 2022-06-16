# Function to generate the dataset necessary for further analysis
list_selected_pol_info <-  function(all_polygons,
                                    intersection,
                                    niveau, 
                                    jaar, 
                                    gemeente_1_incidents,
                                    gemeente_2_incidents, wijken_2_incidents,
                                    gemeente_3_incidents, wijken_3_incidents, buurten_3_incidents){ 
  
  polygons_niveau <- all_polygons %>% 
    filter(Niveau == niveau)  # Only polygons on selected niveau (gemeente, wijk, buurt)
  
  intersection <- intersection %>% 
    filter(Niveau == niveau & JAAR_VKL == jaar)    # Precalculated intersection filtered on niveau and year of input

  df_intersection <- as.data.frame(intersection) 
  df_polygons <- as.data.frame(all_polygons)
  
  if (niveau == "Gemeenten"){
    pol_select <- polygons_niveau %>% filter(GM_NAAM == gemeente_1_incidents)
    incidents_count_niveau <- df_intersection %>% 
      group_by(GM_NAAM) %>% 
      count()
    number_incidents <- incidents_count_niveau %>% 
      filter(GM_NAAM == gemeente_1_incidents)
    intersection_select <- intersection %>% 
      filter(GM_NAAM == gemeente_1_incidents & Niveau == niveau)
    stedelijkheid_num <- df_polygons %>% 
      filter(GM_NAAM == gemeente_1_incidents & Niveau == niveau) %>% 
      select(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
  
  } else if(niveau == "Wijken"){
    pol_select <- polygons_niveau %>% filter(GM_NAAM == gemeente_2_incidents & 
                                             WK_NAAM == wijken_2_incidents)
    incidents_count_niveau <- df_intersection %>% 
      group_by(WK_NAAM, GM_NAAM) %>% 
      count()
    number_incidents <- incidents_count_niveau %>% 
      filter(GM_NAAM == gemeente_2_incidents & 
               WK_NAAM == wijken_2_incidents)
    intersection_select <- intersection %>% 
      filter(GM_NAAM == gemeente_2_incidents & 
               WK_NAAM == wijken_2_incidents & 
               Niveau == niveau)
    stedelijkheid_num <- df_polygons %>% 
      filter(GM_NAAM == gemeente_2_incidents & 
               WK_NAAM == wijken_2_incidents & 
               Niveau == niveau) %>% 
      select(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
  
  } else if (niveau == "Buurten"){
    pol_select <- polygons_niveau %>% filter(GM_NAAM == gemeente_3_incidents & 
                                             WK_NAAM == wijken_3_incidents & 
                                             BU_NAAM == buurten_3_incidents)
    incidents_count_niveau <- df_intersection %>% 
      group_by(BU_NAAM, WK_NAAM, GM_NAAM) %>% 
      count()
    number_incidents <- incidents_count_niveau %>% 
      filter(GM_NAAM == gemeente_3_incidents &
               WK_NAAM == wijken_3_incidents &
               BU_NAAM == buurten_3_incidents)   
    intersection_select <- intersection %>% 
      filter(GM_NAAM == gemeente_3_incidents & 
               WK_NAAM == wijken_3_incidents &
               BU_NAAM == buurten_3_incidents &
               Niveau == "Buurten")
    stedelijkheid_num <- df_polygons %>% 
      filter(GM_NAAM == gemeente_3_incidents &
               WK_NAAM == wijken_3_incidents &
               BU_NAAM == buurten_3_incidents &
               Niveau == "Buurten") %>% 
      select(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
  }

  number_incidents <- number_incidents$n
  number_incidents <- ifelse(is.na(number_incidents), "0", number_incidents)

  stedelijkheid_num <- stedelijkheid_num$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`

  list_ongevallen_return <- list("pol_select" = pol_select,                  # Dataset with selected polygon/area 
                               "intersection_select" = intersection_select,  # All point data of selected area in selected year
                               "number_incidents" = number_incidents,        # Number of incidents in selected area and year 
                               "stedelijkheid_num" = stedelijkheid_num       # Stedelijkheid number for valuebox
  )               

  return(list_ongevallen_return)   
}

# Function to generate the data necessary for top 5
top5_incidents <- function(){
  
  comparable_df <- as.data.frame(intersection) %>% 
    filter(Niveau == input$niveau_incidents & 
             JAAR_VKL == input$jaar &
             `Stedelijkheid..1.zeer.sterk.stedelijk..5.niet.stedelijk.` == list_selected_pol()$stedelijkheid_num) 
  if (input$niveau_incidents == "Gemeenten"){
    incidents_count_niveau <- comparable_df %>% 
      group_by(GM_NAAM, .drop = FALSE) %>% 
      count()
    names_all_niveau <- as.data.frame(all_polygons) %>% 
      filter(Niveau == "Gemeenten" &
               `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)` == list_selected_pol()$stedelijkheid_num)
    no_incidents_niveau <- anti_join(names_all_niveau, incidents_count_niveau, "GM_NAAM") %>% 
      select(GM_NAAM) %>% 
      mutate(n = 0)
    incidents_all_inclu_niveau <- rbind(incidents_count_niveau, no_incidents_niveau)
    sorted <- incidents_count_niveau %>% arrange(desc(n))           # In descending order, so most incidents are at top
    sorted$Rank <- seq.int(nrow(sorted))                            # Add row numbers to dataframe as indication of place in order
    sorted <- sorted %>% relocate(Rank)                             # Rank as first column 
    pol_selected <- sorted %>% filter(GM_NAAM == input$gemeente_1_incidents)  # Selected polygon and number of incidents
    top5_incidents <- head(sorted, 5)
    top5_incidents <- rbind(top5_incidents, pol_selected)
    top5_incidents <- top5_incidents %>% distinct(GM_NAAM, .keep_all = TRUE)
    top5_incidents <- rename(top5_incidents, "Gemeente" = GM_NAAM,
                             "Aantal" = n)
    
    number_incidents <- incidents_count_niveau %>% 
      filter(GM_NAAM == input$gemeente_1_incidents)
    
  } else if(input$niveau_incidents == "Wijken"){
    incidents_count_niveau <- comparable_df %>% 
      group_by(WK_NAAM, GM_NAAM, .drop = FALSE) %>% 
      count()
    names_all_niveau <- as.data.frame(all_polygons) %>% 
      filter(Niveau == "Wijken" &
               `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)` == list_selected_pol()$stedelijkheid_num)
    no_incidents_niveau <- anti_join(names_all_niveau, incidents_count_niveau, by = c("WK_NAAM", "GM_NAAM")) %>% 
      select(WK_NAAM, GM_NAAM) %>% 
      mutate(n = 0)
    incidents_all_inclu_niveau <- rbind(incidents_count_niveau, no_incidents_niveau)
    sorted <- incidents_all_inclu_niveau %>% arrange(desc(n)) 
    sorted$Rank <- seq.int(nrow(sorted))                   
    sorted <- sorted %>% relocate(Rank)
    pol_selected <- sorted %>% filter(GM_NAAM == input$gemeente_2_incidents & 
                                        WK_NAAM == input$wijken_2_incidents)
    top5_incidents <- head(sorted, 5)
    top5_incidents <- rbind(top5_incidents, pol_selected)
    top5_incidents <- top5_incidents %>% distinct(GM_NAAM, WK_NAAM, .keep_all = TRUE)
    top5_incidents <- rename(top5_incidents, "Gemeente" = GM_NAAM,
                             "Wijk" = WK_NAAM,
                             "Aantal" = n)
    number_incidents <- incidents_all_inclu_niveau %>% 
      filter(GM_NAAM == input$gemeente_2_incidents & 
               WK_NAAM == input$wijken_2_incidents)
  } else if(input$niveau_incidents == "Buurten"){
    incidents_count_niveau <- comparable_df %>% 
      group_by(BU_NAAM, WK_NAAM, GM_NAAM, .drop = FALSE) %>% 
      count()
    
    names_all_niveau <- as.data.frame(all_polygons) %>% 
      filter(Niveau == "Buurten" &
               `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)` == list_selected_pol()$stedelijkheid_num)
    no_incidents_niveau <- anti_join(names_all_niveau, incidents_count_niveau, by = c("BU_NAAM", "WK_NAAM", "GM_NAAM")) %>% 
      select(BU_NAAM, WK_NAAM, GM_NAAM) %>% 
      mutate(n = 0)
    incidents_all_inclu_niveau <- rbind(incidents_count_niveau, no_incidents_niveau)
    sorted <- incidents_all_inclu_niveau %>% arrange(desc(n))
    sorted$Rank <- seq.int(nrow(sorted))                   
    sorted <- sorted %>% relocate(Rank) 
    pol_selected <- sorted %>% filter(GM_NAAM == input$gemeente_3_incidents & 
                                        WK_NAAM == input$wijken_3_incidents & 
                                        BU_NAAM == input$buurten_3_incidents)    
    top5_incidents <- head(sorted, 5)
    top5_incidents <- rbind(top5_incidents, pol_selected)
    top5_incidents <- top5_incidents %>% distinct(GM_NAAM, WK_NAAM, BU_NAAM, .keep_all = TRUE)
    
    top5_incidents <- rename(top5_incidents, "Gemeente" = GM_NAAM,
                             "Wijk" = WK_NAAM,
                             "Buurt" = BU_NAAM,
                             "Aantal" = n)
    number_incidents <- incidents_all_inclu_niveau %>% 
      filter(GM_NAAM == input$gemeente_3_incidents & 
               WK_NAAM == input$wijken_3_incidents & 
               BU_NAAM == input$buurten_3_incidents)
    
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
  
  
}
