#Function that takes four column names and creates a barplot of the selected area and the mean of comparable areas
plot4 <- function(dataset, column1, column2, column3){
  
  #Calculating the mean values of the input columns
  df <- as.data.frame(dataset)
  df_gem <- select(df, column1, column2, column3)
  df_gem <- as.data.frame(colMeans(df_gem, na.rm = TRUE))
  df_gem <-  rownames_to_column(df_gem)
  df_gem$groep <- "Gemiddelde"
  df_gem <- rename(df_gem, c(Variabele = 1, Aantal = 2, groep = 3))
  
  #Looking for the values of the input columns from the selected areas
  selected_area_code <- df[1, "selected_area_code"]
  selected_area_label <- df[1, "selected_area_label"]
  df_selected <- df %>% subset(CODE == selected_area_code) %>% 
    select(column1, column2, column3)
  df_selected <- rownames_to_column(as.data.frame(t(df_selected)))
  df_selected$groep <- selected_area_label
  df_selected <- rename(df_selected, c(Variabele = 1, Aantal = 2, groep = 3))
  
  #adding mean and selected together
  df_final <- rbind(df_selected, df_gem)
  df_final$groep <- as.character(df_final$groep)
  df_final$groep <- factor(df_final$groep, levels = unique(df_final$groep))
  df_final$Variabele <- as.character(df_final$Variabele)
  df_final$Variabele <- factor(df_final$Variabele, levels = unique(df_final$Variabele))
  
  #Plot
  plot <- ggplot(df_final, aes(x = Variabele, y = Aantal, fill = groep)) + 
    geom_col(position = "dodge") + 
    theme(text = element_text(size = 14), legend.title = element_blank(), legend.position="top")+
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15))
  return(plot)
}

#Maps for the amount of instances inside a radius
plot_4_theme <- function(data, subthema){

  dataset <- data
  if (subthema == "Huisartsenpraktijk"){
    plot4(dataset,
          "Aantal huisartsenpraktijken binnen 1 km", 
          "Aantal huisartsenpraktijken binnen 3 km", 
          "Aantal huisartsenpraktijken binnen 5 km")
  }else if (subthema == "Ziekenhuis incl. buitenpolikliniek"){
    plot4(dataset,
          "Aantal ziekenhuizen incl. buitenpolikliniek binnen 5 km",                        
          "Aantal ziekenhuizen incl. buitenpolikliniek binnen 10 km",                       
          "Aantal ziekenhuizen incl. buitenpolikliniek binnen 20 km")
  }else if (subthema == "Ziekenhuis excl. buitenpolikliniek"){
    plot4(dataset,
          "Aantal ziekenhuizen excl. Buitenpolikliniek binnen 5 km",                        
          "Aantal ziekenhuizen excl. Buitenpolikliniek binnen 10 km",                       
          "Aantal ziekenhuizen excl. Buitenpolikliniek binnen 20 km")
  }else if (subthema == "Supermarkt"){
    plot4(dataset,
          "Aantal  grote supermarkten binnen 1 km",                                         
          "Aantal  grote supermarkten binnen 3 km",                                          
          "Aantal  grote supermarkten binnen 5 km")
  }else if (subthema == "Overige dagelijkse levensmiddelen"){
    plot4(dataset,
          "Aantal winkels overige dagelijkse levensmiddelen binnen 1 km",                    
          "Aantal winkels overige dagelijkse levensmiddelen binnen 3 km",                    
          "Aantal winkels overige dagelijkse levensmiddelen binnen 5 km")
  }else if (subthema == "Warenhuis"){
    plot4(dataset,
          "Aantal warenhuizen binnen 5 km",                                                  
          "Aantal warenhuizen binnen 10 km",                                                
          "Aantal warenhuizen binnen 20 km")
  }else if (subthema == "Café"){
    plot4(dataset,
          "Aantal cafes binnen 1 km" ,                                                       
          "Aantal cafes binnen 3 km" ,                                                       
          "Aantal cafes binnen 5 km")
  }else if (subthema == "Cafetaria"){
    plot4(dataset,
          "Aantal cafetaria's binnen 1 km",                                                  
          "Aantal cafetaria's binnen 3 km",                                                  
          "Aantal cafetaria's binnen 5 km")
  }else if (subthema == "Restaurant"){
    plot4(dataset,
          "Aantal restaurants binnen 1 km",                                                  
          "Aantal restaurants binnen 3 km",                                                  
          "Aantal restaurants binnen 5 km")
  }else if (subthema == "Hotel"){
    plot4(dataset,
          "Aantal hotel binnen 5 km",                                                  
          "Aantal hotel binnen 10 km",                                                  
          "Aantal hotel binnen 20 km")
  }else if (subthema == "Kinderdagverblijf"){
    plot4(dataset,
          "Aantal kinderdagverblijf  binnen 1 km",                                                  
          "Aantal kinderdagverblijf  binnen 3 km",                                                  
          "Aantal kinderdagverblijf  binnen 5 km")
  }else if (subthema == "Buitenschoolse opvang"){
    plot4(dataset,
          "Aantal buitenschoolse opvang  binnen 1 km",                                                  
          "Aantal buitenschoolse opvang  binnen 3 km",                                                  
          "Aantal buitenschoolse opvang  binnen 5 km")
  }else if (subthema == "Basisschool"){
    plot4(dataset,
          "Aantal basisscholen binnen 1 km",                                                  
          "Aantal basisscholen binnen 3 km",                                                  
          "Aantal basisscholen binnen 5 km")
  }else if (subthema == "Voortgezet onderwijs"){
    plot4(dataset,
          "Aantal voortgezet onderwijs binnen 3 km",                                                  
          "Aantal voortgezet onderwijs binnen 5 km",                                                  
          "Aantal voortgezet onderwijs binnen 10 km")
  }else if (subthema == "VMBO school"){
    plot4(dataset,
          "Aantal scholen VMBO binnen 3 km",                                                  
          "Aantal scholen VMBO binnen 5 km",                                                  
          "Aantal scholen VMBO binnen 10 km")
  }else if (subthema == "HAVO/VWO school"){
    plot4(dataset,
          "Aantal scholen HAVO/VWO binnen 3 km",                                                  
          "Aantal scholen HAVO/VWO binnen 5 km",                                                  
          "Aantal scholen HAVO/VWO binnen 10 km")
  }else if (subthema == "Bioscoop"){
    plot4(dataset,
          "Aantal bioscoop binnen 5 km",                                                  
          "Aantal bioscoop binnen 10 km",                                                  
          "Aantal bioscoop binnen 20 km")
  }else if (subthema == "Attractie"){
    plot4(dataset,
          "Aantal attracties binnen 10 km",                                                  
          "Aantal attracties binnen 20 km",                                                  
          "Aantal attracties binnen 50 km")
  }else if (subthema == "Podiumkunsten"){
    plot4(dataset,
          "Aantal podiumkunsten binnen 5 km",                                                  
          "Aantal podiumkunsten binnen 10 km",                                                  
          "Aantal podiumkunsten binnen 20 km")
  }else if (subthema == "Museum"){
    plot4(dataset,
          "Aantal musea binnen 5 km",                                                  
          "Aantal musea binnen 10 km",                                                  
          "Aantal musea binnen 20 km")
  }
}