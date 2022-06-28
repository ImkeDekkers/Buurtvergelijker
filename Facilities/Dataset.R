# Function that filters the full_data such that only the right areas remain in the dataset
# What the right areas are depend on the selected level, area and what the user wants to compare with

dataset <- function(full_data, niveau, vergelijkbaar1, gemeente1, vergelijkbaar2, gemeente2, wijken2,  
                    vergelijkbaar3, gemeente3, wijken3, buurten3){
  df <- as.data.frame(full_data)
  df <- df[df$Niveau == niveau,]
  if(niveau == 'Gemeenten'){
    if(vergelijkbaar1 == "Stedelijkheidsniveau"){
      stedelijkheid_num <- df[df$GM_NAAM == gemeente1, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
      comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
    }else if (vergelijkbaar1 == "Inkomensniveau"){
      inkomen_num <- df[df$GM_NAAM == gemeente1, 'inkomengroep']
      comparable_df <- df[df$inkomengroep == inkomen_num, ]
    }else if(vergelijkbaar1 == "Nederland"){
      comparable_df <- df
    }
    comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == gemeente1) %>% pull(CODE)
    comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == gemeente1) %>% pull(NAAM)
    selected_polygon <- comparable_df %>% filter(GM_NAAM == gemeente1)
    row_num_selected <- which(comparable_df$GM_NAAM == gemeente1)
    ink_level <- "bekend"
  }else if(niveau == 'Wijken'){
    if(vergelijkbaar2 == "Stedelijkheidsniveau"){
      stedelijkheid_num <- df[df$WK_NAAM == wijken2 & df$GM_NAAM == gemeente2, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
      comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
      comparable_df <- comparable_df %>% drop_na(CODE)
      ink_level <- "bekend"
    }else if (vergelijkbaar2 == "Inkomensniveau"){
      inkomen_num <- df[df$WK_NAAM == wijken2 & df$GM_NAAM == gemeente2, 'inkomengroep']
      if(is.na(inkomen_num)){
        comparable_df <- df[df$Niveau == niveau,]
        ink_level <- "onbekend"
      }else{
        comparable_df <- df[df$inkomengroep == inkomen_num, ]
        comparable_df <- comparable_df %>% drop_na(CODE)
        ink_level <- "bekend"
      }
    }else if(vergelijkbaar2 == "Nederland"){
      comparable_df <- df
      ink_level <- "bekend"
 
    }
    comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == gemeente2 & WK_NAAM == wijken2) %>%pull(CODE)
    comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == gemeente2 & WK_NAAM == wijken2) %>%pull(NAAM)
    selected_polygon <- comparable_df %>% filter(GM_NAAM == gemeente2 & WK_NAAM == wijken2)
    row_num_selected <- which(comparable_df$GM_NAAM == gemeente2 & comparable_df$WK_NAAM == wijken2)
  }else if(niveau == 'Buurten'){
    ink_level <- "bekend"
    if(vergelijkbaar3 == "Stedelijkheidsniveau"){
      stedelijkheid_num <- df[df$BU_NAAM==buurten3 & df$GM_NAAM == gemeente3 & df$WK_NAAM == wijken3, "Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)"]
      comparable_df <- df[df$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]
      comparable_df <- comparable_df %>% drop_na(CODE)
    } else if(vergelijkbaar3 == "Nederland"){
      comparable_df <- df
    }
    comparable_df$selected_area_code <- comparable_df %>% filter(GM_NAAM == gemeente3 & WK_NAAM == wijken3 & BU_NAAM == buurten3) %>%pull(CODE)
    comparable_df$selected_area_label <- comparable_df %>% filter(GM_NAAM == gemeente3 & WK_NAAM == wijken3 & BU_NAAM == buurten3) %>%pull(NAAM)
    selected_polygon <- comparable_df %>% filter(GM_NAAM == gemeente3 & WK_NAAM == wijken3 & BU_NAAM == buurten3)
    row_num_selected <- which(comparable_df$GM_NAAM == gemeente3 & comparable_df$WK_NAAM == wijken3 & comparable_df$BU_NAAM == buurten3)
  } 
  comparable_df$centroidxx <- comparable_df[row_num_selected, 'centroidx']
  comparable_df$centroidyy <- comparable_df[row_num_selected, 'centroidy']
  dataset <- st_as_sf(comparable_df)
  
  list_return <- list("dataset" = dataset, "selected_polygon" = selected_polygon, "ink_level"=ink_level)
  return(list_return)
}