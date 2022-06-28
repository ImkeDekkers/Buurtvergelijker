source("../Health/SimilarAgeDistribution.R")

# Function that filters the data gezondheid_all such that only the right areas remain in the dataset
# What the right areas are depend on the selected level, area and what the user wants to compare with

dataset_gez <- function(gezondheid_all, niveau_gez, vergelijkbaar1_gez, gemeente1_gez,
                             vergelijkbaar2_gez, gemeente2_gez, wijken2_gez,
                             vergelijkbaar3_gez, gemeente3_gez, wijken3_gez, buurten3_gez){
  
  df <- as.data.frame(gezondheid_all)
  df <- df[df$Niveau == niveau_gez,]
  if(niveau_gez == 'Gemeenten'){
    if(vergelijkbaar1_gez == "Stedelijkheidsniveau"){
      stedelijkheid_num <- df %>% filter(GM_NAAM == gemeente1_gez) %>% pull(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
      stedelijkheid_num <- stedelijkheid_num[1]
      comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
    }else if (vergelijkbaar1_gez == "Inkomensniveau"){
      inkomen_num <- df %>% filter(GM_NAAM == gemeente1_gez) %>% pull(`inkomengroep`)         
      inkomen_num<-inkomen_num[1]
      comparable_df <- df[df$inkomengroep == inkomen_num, ]
    }else if(vergelijkbaar1_gez == "Nederland"){
      comparable_df <- df
    }else if(vergelijkbaar1_gez == "age_distribution"){
      comparable_df <- df
    }
    selected_area_code <- comparable_df %>% filter(GM_NAAM == gemeente1_gez) %>% pull(CODE)
    comparable_df$selected_area_code <- selected_area_code[1]
    if(vergelijkbaar1_gez=="age_distribution"){
      comparable_df <- similar_age(comparable_df, niveau_gez)
      selected_area_code <- comparable_df %>% filter(GM_NAAM == gemeente1_gez) %>% pull(CODE)
      comparable_df$selected_area_code <- selected_area_code[1]
    }
    selected_area_label <- comparable_df %>% filter(GM_NAAM == gemeente1_gez) %>% pull(NAAM)
    comparable_df$selected_area_label <- selected_area_label[1]
    selected_polygon <- comparable_df %>% filter(GM_NAAM == gemeente1_gez)
    selected_polygon <- selected_polygon[1,]
    row_num_selected <- which(comparable_df$GM_NAAM == gemeente1_gez)
    row_num_selected <-row_num_selected[1]
    ink_age_level <- "bekend"
  }else if(niveau_gez == 'Wijken'){
    if(vergelijkbaar2_gez == "Stedelijkheidsniveau"){
      stedelijkheid_num <- df %>% filter(WK_NAAM == wijken2_gez & GM_NAAM == gemeente2_gez) %>% pull(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
      stedelijkheid_num <- stedelijkheid_num[1]
      comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
      comparable_df <- comparable_df %>% drop_na(CODE)
      ink_age_level <- "bekend"
    }else if (vergelijkbaar2_gez == "Inkomensniveau"){
      inkomen_num <-df %>% filter(WK_NAAM == wijken2_gez & GM_NAAM == gemeente2_gez) %>% pull(`inkomengroep`)
      inkomen_num<-inkomen_num[1]
      if(is.na(inkomen_num)){
        comparable_df <- df[df$Niveau == niveau_gez,]
        ink_age_level <- "ink_onbekend"
      }else{
        comparable_df <- df[df$inkomengroep == inkomen_num, ]
        comparable_df <- comparable_df %>% drop_na(CODE)
        ink_age_level <- "bekend"
      }
    }else if(vergelijkbaar2_gez == "Nederland"){
      comparable_df <- df
      ink_age_level <- "bekend"
    }else if(vergelijkbaar2_gez == "age_distribution"){
      comparable_df <- df
    }
    selected_area_code <- comparable_df %>% filter(GM_NAAM == gemeente2_gez & WK_NAAM == wijken2_gez) %>%pull(CODE)
    comparable_df$selected_area_code <- selected_area_code[1]
    age_0_15 <- comparable_df %>% filter(GM_NAAM == gemeente2_gez & WK_NAAM == wijken2_gez) %>%pull(`Personen 0 tot 15 jaar (%)`)
    age_0_15 <- age_0_15[1]
    if(vergelijkbaar2_gez == "age_distribution"){
      if(is.na(age_0_15)){
        comparable_df <- df 
        ink_age_level <- "age_onbekend"
      }else{
        comparable_df <- similar_age(comparable_df, input$niveau_gez)
        ink_age_level <- "bekend"
      }
      selected_area_code <- comparable_df %>% filter(GM_NAAM == gemeente2_gez & WK_NAAM == wijken2_gez) %>%pull(CODE)
      comparable_df$selected_area_code <- selected_area_code[1]
    }
    selected_area_label <- comparable_df %>% filter(GM_NAAM == gemeente2_gez & WK_NAAM == wijken2_gez) %>%pull(NAAM)
    comparable_df$selected_area_label <- selected_area_label[1]
    selected_polygon <- comparable_df %>% filter(GM_NAAM == gemeente2_gez & WK_NAAM == wijken2_gez)
    selected_polygon <- selected_polygon[1,]
    row_num_selected <- which(comparable_df$GM_NAAM == gemeente2_gez & comparable_df$WK_NAAM == wijken2_gez)
    row_num_selected <-row_num_selected[1]
  }else if(niveau_gez == 'Buurten'){
    if(vergelijkbaar3_gez == "Stedelijkheidsniveau"){
      stedelijkheid_num <- df%>% filter(BU_NAAM==buurten3_gez & GM_NAAM == gemeente3_gez & WK_NAAM == wijken3_gez) %>% pull(`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)
      stedelijkheid_num <- stedelijkheid_num[1]
      comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
      comparable_df <- comparable_df %>% drop_na(CODE)
      ink_age_level <- "bekend"
    } else if(vergelijkbaar3_gez == "Nederland"){
      comparable_df <- df
      ink_age_level <- "bekend"
    }else if(vergelijkbaar3_gez == "age_distribution"){
      comparable_df <- df
    }
    selected_area_code <- comparable_df %>% filter(GM_NAAM == gemeente3_gez & WK_NAAM == wijken3_gez & BU_NAAM == buurten3_gez) %>%pull(CODE)
    comparable_df$selected_area_code <- selected_area_code[1]
    age_0_15 <- comparable_df %>% filter(GM_NAAM == gemeente3_gez & WK_NAAM == wijken3_gez & BU_NAAM == buurten3_gez) %>%pull(`Personen 0 tot 15 jaar (%)`)
    age_0_15 <- age_0_15[1]
    if(vergelijkbaar3_gez == "age_distribution"){
      if(is.na(age_0_15)){
        comparable_df <- df 
        ink_age_level <- "age_onbekend"
      }else{
        comparable_df <- similar_age(comparable_df, input$niveau_gez)
        ink_age_level <- "bekend"
      }
      selected_area_code <- comparable_df %>% filter(GM_NAAM == gemeente3_gez & WK_NAAM == wijken3_gez & BU_NAAM == buurten3_gez) %>%pull(CODE)
      comparable_df$selected_area_code <- selected_area_code[1]
    }
    selected_area_label <- comparable_df %>% filter(GM_NAAM == gemeente3_gez & WK_NAAM == wijken3_gez & BU_NAAM == buurten3_gez) %>%pull(NAAM)
    comparable_df$selected_area_label <- selected_area_label[1]
    selected_polygon <- comparable_df %>% filter(GM_NAAM == gemeente3_gez & WK_NAAM == wijken3_gez & BU_NAAM == buurten3_gez)
    selected_polygon <- selected_polygon[1,]
    row_num_selected <- which(comparable_df$GM_NAAM == gemeente3_gez & comparable_df$WK_NAAM == wijken3_gez & comparable_df$BU_NAAM == buurten3_gez)
    row_num_selected <-row_num_selected[1]
    
  }
  comparable_df$centroidxx <- comparable_df[row_num_selected, 'centroidx']
  comparable_df$centroidyy <- comparable_df[row_num_selected, 'centroidy']
  dataset <- st_as_sf(comparable_df)
  
  list_return <- list("dataset" = dataset, "selected_polygon" = selected_polygon, "ink_age_level"=ink_age_level)
  return(list_return)
}