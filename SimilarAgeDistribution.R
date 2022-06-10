# Function that returns the similar areas as the selected area based on the age distribution.
# An area is defined to be similar if the total distance to all age variables is 10 or less.
# When there are less than 5 similar areas, the 5 most similar areas are returned.
# When the amount of similar areas is more than 20% of all areas on the same level, the 20% that are most similar are returned.

similar_age <- function(data, niveau){
  df_original <- as.data.frame(data)
  df <- df_original[df_original$Perioden=="2020" & df_original$Leeftijd=="18+",]
  df <- df %>% drop_na(Perioden)
  
  #Making CODE the row index so all rows are identifiable 
  result <-  subset(df, select = -c(CODE))
  row.names(result) <- df$CODE
  
  #subset df so it only contains age variables
  result <- subset(result, select= `Personen 0 tot 15 jaar (%)`: `Personen 65 jaar en ouder (%)`)
  
  #Making a df for the selected area 
  selected_area_code <- df[1, "selected_area_code"]
  selected_area <- result[rownames(result) == selected_area_code,]
  
  #Calculating distance from the selected area to all areas
  dist_matrix <- apply(selected_area,1,function(selected_area)apply(result,1,function(result,selected_area)dist(rbind(result,selected_area),method = 'manhattan'),selected_area))
  dist_df <- as.data.frame(dist_matrix)
  colnames(dist_df) <- "afstand"
  dist_df$CODE <- row.names(dist_df)
  
  #Defining cutoff distance for similar areas and take only areas within cutoff distance
  if(niveau=="Gemeenten"){
    value <- 10
  }else if(niveau=="Wijken"){
    value <- 10
  }else if(niveau=="Buurten"){
    value <- 10
  }
  top <-dist_df[dist_df$afstand <= value, ]
  
  #If there are less than 5 areas with a similar age distribution, take 5 closest areas
  if(nrow(top)<=6){
    sorted <-  dist_df[order(dist_df$`afstand`),]
    top <- head(sorted, 6)
  }
  #If the number of areas with a similar age distribution is bigger than 20% of the areas on the same level, take closest 20%
  else if(nrow(top)>round(nrow(dist_df)*0.2)){
    sorted <-  dist_df[order(dist_df$`afstand`),]
    top <- head(sorted, round(nrow(sorted)*0.2))
  }
  #Merging with original df to get the names of the areas
  final <- merge(top, df, by="CODE")
  
  #Keep only the areas in the original dataframe that are in the top 20% similar areas
  similar_codes <- unique(final$CODE)
  df_final <- df_original%>% filter(CODE %in% similar_codes)
  
  return(df_final)
}