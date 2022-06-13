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
  ggplot(df_final, aes(x = Variabele, y = Aantal, fill = groep)) + 
    geom_col(position = "dodge") + 
    theme(text = element_text(size = 14), legend.title = element_blank(), legend.position="top")+
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15))
}