# Function that returns the 5 most similar areas to the input area based on all voorzieningen variables
top5_distances_overall <- function(dataset){
  
  df <- as.data.frame(dataset)
  
  # Making CODE the row index so all rows are identifiable 
  result <-  subset(df, select = -c(CODE))
  row.names(result) <- df$CODE
  
  # Subset df so it only contains voorzieningen variables and transform the data with z-score (scale function)
  result <- subset(result, select= `Afstand tot huisartsenpraktijk (km)`: `Aantal musea binnen 20 km`)
  normalized <- as.data.frame(scale(result))
  
  # Making one df for the selected area and one for the comparable areas without the area itself 
  selected_area_code <- df[1, "selected_area_code"]
  selected_area <- normalized[rownames(normalized) == selected_area_code, ]
  other <- normalized[rownames(normalized) != selected_area_code, ]
  
  # Calculating distance from the selected area to all areas in other dataframe
  dist_matrix <- apply(selected_area, 1, 
                       function(selected_area)apply(other, 1,
                                                    function(other, selected_area)dist(rbind(other, selected_area),
                                                                                       method = 'manhattan'),
                                                    selected_area))
  dist_df <- as.data.frame(dist_matrix)
  dist_df <- rename(dist_df, "afstand" = 1)
  dist_df$CODE <- row.names(dist_df)
  
  # Ordering the distances and returning top 5
  sorted <-  dist_df[order(dist_df$`afstand`), ]
  top5 <- head(sorted, 5)
  
  # Merging with original df to get the names of the areas
  final <- merge(top5, df, by = "CODE")
  final <-  final[order(final$`afstand`), ]      #After merge it was not sorted anymore
  
  return(final)
}

table_top5_distances_overall <- function(dataset, input_niveau_facilities){
  final <- top5_distances_overall(dataset)
  
  # If there are no data about the distance to the amenities, return message
  if (is.na(final$afstand)){
    return("Er zijn onvoldoende gegevens beschikbaar voor het geselecteerde gebied")
    
    # Returning GM_NAAM, WK_NAAM and BU_NAAM based on selected niveau
  }else if(input_niveau_facilities == "Gemeenten"){
    final <- final %>% select(GM_NAAM)
    final <- rename(final, "Gemeente naam" = GM_NAAM)
    row.names(final) <- NULL
  }else if (input_niveau_facilities == "Wijken"){
    final <- final %>% unite(., col = "Wijk naam",  WK_NAAM, GM_NAAM, na.rm = TRUE, sep = " (gemeente ")
    final$`Wijk naam` <- paste0(final$`Wijk naam`, ")")
    final <- final %>% select(`Wijk naam`)
    row.names(final) <- NULL
  }else if (input_niveau_facilities == "Buurten"){
    final <- final %>% unite(., col = "Buurt naam",  BU_NAAM, GM_NAAM, na.rm = TRUE, sep = " (gemeente ")
    final <- final %>% unite(., col = "Buurt naam",  `Buurt naam`, WK_NAAM, na.rm = TRUE, sep = ", wijk ")
    final$`Buurt naam` <- paste0(final$`Buurt naam`, ")")
    final <- final %>% select(`Buurt naam`)
    row.names(final) <- NULL
  }
  return(final)
}

# Function that returns the 5 most similar areas to the input area based on voorzieningen variables in chosen theme
top5_distances_theme <- function(dataset, input_niveau_facilities, thema){
  
  df <- as.data.frame(dataset)
  
  # Making CODE the row index so all rows are identifiable 
  result <-  subset(df, select = -c(CODE))
  row.names(result) <- df$CODE
  
  thema <- thema

  # Subset df based on theme input so it contains the right voorzieningen variables and transform the data with z-score (scale function)
  if(thema == "Gezondheid en welzijn"){
    result <- subset(result, select = `Afstand tot huisartsenpraktijk (km)`: `Aantal ziekenhuizen excl. Buitenpolikliniek binnen 20 km`)
  }else if(thema == "Detailhandel"){
    result <- subset(result, select = `Afstand tot grote supermarkt (km)`: `Aantal warenhuizen binnen 20 km`)
  }else if(thema == "Horeca"){
    result <- subset(result, select = `Afstand tot cafe (km)`: `Aantal hotel binnen 20 km`)
  }else if(thema == "Kinderopvang"){
    result <- subset(result, select = `Afstand tot kinderdagverblijf  (km)`: `Aantal buitenschoolse opvang  binnen 5 km`)
  }else if(thema == "Onderwijs"){
    result <- subset(result, select = `Afstand tot basisscholen (km)`: `Aantal scholen HAVO/VWO binnen 10 km`)
  }else if(thema == "Verkeer en vervoer"){
    result <- subset(result, select = `Afstand tot oprit hoofdverkeersweg (km)`: `Afstand tot belangrijk overstapstation (km)`)
  }else if (thema == "Vrije tijd en cultuur"){
    result <- subset(result, select = `Afstand tot zwembad (km)`: `Aantal musea binnen 20 km`)
  }
  normalized <- as.data.frame(scale(result))
  
  # Making one df for the selected area and one for the comparable areas without the area itself 
  selected_area_code <- df[1, "selected_area_code"]
  selected_area <- normalized[rownames(normalized) == selected_area_code, ]
  other <- normalized[rownames(normalized) != selected_area_code, ]
  
  # Calculating distance from the selected area to all areas in other dataframe
  dist_matrix <- apply(selected_area, 1, 
                       function(selected_area)apply(other, 1,
                                                    function(other,selected_area)dist(rbind(other,selected_area),
                                                                                      method = 'manhattan'),
                                                    selected_area))
  dist_df <- as.data.frame(dist_matrix)
  dist_df <- rename(dist_df, "afstand" = 1)
  dist_df$CODE <- row.names(dist_df)
  
  # Ordering the distances and returning top 5
  sorted <-  dist_df[order(dist_df$`afstand`), ]
  top5 <- head(sorted, 5)
  
  # Merging with original df to get the names of the areas
  final <- merge(top5, df, by = "CODE")
  final <-  final[order(final$`afstand`), ]      #After merge it was not sorted anymore
  
  return(final)
}

table_top5_distances_theme <- function(dataset, input_niveau_facilities, thema){
  final <- top5_distances_theme(dataset, input_niveau_facilities, thema)
  # If there are no data about the distance to the amenities, return message
  if (is.na(final$afstand)){
    return("Er zijn onvoldoende gegevens beschikbaar voor het geselecteerde gebied")
    
    # Returning GM_NAAM, WK_NAAM and BU_NAAM based on selected niveau
  } else if(input_niveau_facilities == "Gemeenten"){
    final <- final %>% select(GM_NAAM)
    final <- rename(final, "Gemeente naam" = GM_NAAM)
    row.names(final) <- NULL
  }else if (input_niveau_facilities == "Wijken"){
    final <- final %>% unite(., col = "Wijk naam",  WK_NAAM, GM_NAAM, na.rm = TRUE, sep = " (gemeente ")
    final$`Wijk naam` <- paste0(final$`Wijk naam`, ")")
    final <- final %>% select(`Wijk naam`)
    row.names(final) <- NULL
  }else if (input_niveau_facilities == "Buurten"){
    final <- final %>% unite(., col = "Buurt naam",  BU_NAAM, GM_NAAM, na.rm = TRUE, sep = " (gemeente ")
    final <- final %>% unite(., col = "Buurt naam",  `Buurt naam`, WK_NAAM, na.rm = TRUE, sep = ", wijk ")
    final$`Buurt naam` <- paste0(final$`Buurt naam`, ")")
    final <- final %>% select(`Buurt naam`)
    row.names(final) <- NULL
  }
  return(final)
}
