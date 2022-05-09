options( scipen = 999 )  #to get rid of the scientific notation in the tables
#Loading libraries
library(dplyr)
library(sf)
library("readxl")
#devtools::install_github('rstudio/leaflet')
library(leaflet)
library(tidyverse)
library('rmapshaper')

#Reading Kerncijfers gemeenten, wijken, buurten 2020
kerncijfers_gemeenten2020 <- st_read("../Data/WijkBuurtkaart_2020_v2/gemeente_2020_v2.shp")
kerncijfers_wijken2020 <- st_read("../Data/WijkBuurtkaart_2020_v2/wijk_2020_v2.shp")
kerncijfers_buurten2020 <- st_read("../Data/WijkBuurtkaart_2020_v2/buurt_2020_v2.shp")

#Reading missing kerncijfers data about inkomen, opleiding, geboorte
ink_opl_geb <- read_xlsx("../Data/Inkomen_opleiding_geboorte.xlsx")
ink_opl_geb <- select(ink_opl_geb, -c(ID))
ink_opl_geb[, c(2:21)] <- sapply(ink_opl_geb[, c(2:21)], as.numeric)

#Cleaning Kerncijfers data
kerncijfers_gemeenten2020 <- kerncijfers_gemeenten2020[kerncijfers_gemeenten2020$H2O == "NEE", ]  
kerncijfers_wijken2020 <- kerncijfers_wijken2020[kerncijfers_wijken2020$H2O == "NEE", ]  
kerncijfers_buurten2020 <- kerncijfers_buurten2020[kerncijfers_buurten2020$H2O == "NEE", ]

#Removing columns with only zero values
kerncijfers_gemeenten2020 <- as.data.frame(kerncijfers_gemeenten2020)
gem_only_0<-dplyr::select_if(kerncijfers_gemeenten2020, is.numeric)
gem_only_0 <- gem_only_0[,colSums(gem_only_0 != 0) ==0]
kerncijfers_gemeenten2020 <- kerncijfers_gemeenten2020[, !colnames(kerncijfers_gemeenten2020) %in% colnames(gem_only_0)]
kerncijfers_gemeenten2020 <- st_as_sf(kerncijfers_gemeenten2020)

kerncijfers_wijken2020 <- as.data.frame(kerncijfers_wijken2020)
buurt_only_0<-dplyr::select_if(kerncijfers_wijken2020, is.numeric)
buurt_only_0 <- buurt_only_0[,colSums(buurt_only_0 != 0) ==0]
kerncijfers_wijken2020 <- kerncijfers_wijken2020[, !colnames(kerncijfers_wijken2020) %in% colnames(buurt_only_0)]
kerncijfers_wijken2020 <- st_as_sf(kerncijfers_wijken2020)

kerncijfers_buurten2020 <- as.data.frame(kerncijfers_buurten2020)
buurt_only_0<-dplyr::select_if(kerncijfers_buurten2020, is.numeric)
buurt_only_0 <- buurt_only_0[,colSums(buurt_only_0 != 0) ==0]
kerncijfers_buurten2020 <- kerncijfers_buurten2020[, !colnames(kerncijfers_buurten2020) %in% colnames(buurt_only_0)]
kerncijfers_buurten2020 <- st_as_sf(kerncijfers_buurten2020)

#Merging kerncijfers and ink_opl_geb
kerncijfers_gemeenten2020 <- left_join(kerncijfers_gemeenten2020, ink_opl_geb, by = c("GM_CODE"= "WijkenEnBuurten"),suffix = c("", ""))
kerncijfers_wijken2020 <- left_join(kerncijfers_wijken2020, ink_opl_geb, by = c("WK_CODE"= "WijkenEnBuurten"),suffix = c("", ""))
kerncijfers_buurten2020 <- left_join(kerncijfers_buurten2020, ink_opl_geb, by = c("BU_CODE"= "WijkenEnBuurten"),suffix = c("", ""))

#Transforming gemeenten, wijken en buurten dataset to CRS 4326 (om leaflet functie werkend te krijgen)
gemeenten2020 <- st_transform(kerncijfers_gemeenten2020, "+init=epsg:4326")
wijken2020 <- st_transform(kerncijfers_wijken2020, "+init=epsg:4326")
buurten2020 <- st_transform(kerncijfers_buurten2020, "+init=epsg:4326")

#Change missing values from -99999999 to NA
gemeenten2020 <- gemeenten2020 %>%
  mutate(across(where(is.numeric), ~na_if(.,-99999999)))
wijken2020 <- wijken2020 %>%
  mutate(across(where(is.numeric), ~na_if(.,-99999999)))
buurten2020 <- buurten2020 %>%
  mutate(across(where(is.numeric), ~na_if(.,-99999999)))

#simplifying the geometries so loading the map goes faster
gemeenten <- rmapshaper::ms_simplify(gemeenten2020, keep = 0.05, keep_shapes = TRUE)
wijken <- rmapshaper::ms_simplify(wijken2020, keep = 0.05, keep_shapes = TRUE)
buurten <- rmapshaper::ms_simplify(buurten2020, keep = 0.05, keep_shapes = TRUE)

#reading all data about the postcodes
postcodes <- read_excel("../Data/2020-cbs-pc6huisnr20200801-buurt/pc6-gwb2020.xlsx")
gem_code <- read_excel("../Data/2020-cbs-pc6huisnr20200801-buurt/gem2020.xlsx")
wijk_code <- read_excel("../Data/2020-cbs-pc6huisnr20200801-buurt/wijk2020.xlsx")
buurt_code <- read_excel("../Data/2020-cbs-pc6huisnr20200801-buurt/brt2020.xlsx")

#joining postcodes information
postcodes_final <- left_join(postcodes, gem_code, by = c("Gemeente2020" = "Gemcode2020"))
postcodes_final <- left_join(postcodes_final, wijk_code, by = c("Wijk2020" = "wijkcode2020"))
postcodes_final <- left_join(postcodes_final, buurt_code, by = c("Buurt2020" = "buurtcode2020"))

#Add WK_NAAM to buurten
buurten <- as.data.frame(buurten)
wijken <- as.data.frame(wijken)

nameswijken <- wijken %>%
  select(WK_CODE, WK_NAAM)

buurten <- left_join(buurten, nameswijken, by = "WK_CODE",suffix = c("", "_new"))
buurten <- st_as_sf(buurten)
wijken <- st_as_sf(wijken)

# Create quantiles for comparability based on income
## Gemeenten
quants_gem_ink <- quantile(gemeenten$HuishOnderOfRondSociaalMinimum_79)

gemeenten <- gemeenten %>% 
  mutate(inkomengroep = case_when(HuishOnderOfRondSociaalMinimum_79 >= quants_gem_ink[1] & 
                                    HuishOnderOfRondSociaalMinimum_79 < quants_gem_ink[2] ~ 1,
                                  HuishOnderOfRondSociaalMinimum_79 >= quants_gem_ink[2] & 
                                    HuishOnderOfRondSociaalMinimum_79 < quants_gem_ink[3] ~ 2,
                                  HuishOnderOfRondSociaalMinimum_79 >= quants_gem_ink[3] & 
                                    HuishOnderOfRondSociaalMinimum_79 < quants_gem_ink[4] ~ 3,
                                  HuishOnderOfRondSociaalMinimum_79 >= quants_gem_ink[4] & 
                                    HuishOnderOfRondSociaalMinimum_79 <= quants_gem_ink[5] ~ 4))

## Wijken 
quants_wk_ink <- quantile(wijken$HuishOnderOfRondSociaalMinimum_79, na.rm = T)

wijken <- wijken %>% 
  mutate(inkomengroep = case_when(HuishOnderOfRondSociaalMinimum_79 >= quants_wk_ink[1] & 
                                    HuishOnderOfRondSociaalMinimum_79 < quants_wk_ink[2] ~ 1,
                                  HuishOnderOfRondSociaalMinimum_79 >= quants_wk_ink[2] & 
                                    HuishOnderOfRondSociaalMinimum_79 < quants_wk_ink[3] ~ 2,
                                  HuishOnderOfRondSociaalMinimum_79 >= quants_wk_ink[3] & 
                                    HuishOnderOfRondSociaalMinimum_79 < quants_wk_ink[4] ~ 3,
                                  HuishOnderOfRondSociaalMinimum_79 >= quants_wk_ink[4] & 
                                    HuishOnderOfRondSociaalMinimum_79 <= quants_wk_ink[5] ~ 4))


# Create quantiles for comparability based on education (opleiding)
## Gemeenten
gemeenten <- gemeenten %>% 
  mutate(perc_opleiding = OpleidingsniveauLaag_64/AANT_INW*100)

quants_gem_opl <- quantile(gemeenten$perc_opleiding)

gemeenten <- gemeenten %>% 
  mutate(opleidingsgroep = case_when(perc_opleiding >= quants_gem_opl[1] & 
                                       perc_opleiding < quants_gem_opl[2] ~ 1,
                                     perc_opleiding >= quants_gem_opl[2] & 
                                       perc_opleiding < quants_gem_opl[3] ~ 2,
                                     perc_opleiding >= quants_gem_opl[3] & 
                                       perc_opleiding < quants_gem_opl[4] ~ 3,
                                     perc_opleiding >= quants_gem_opl[4] & 
                                       perc_opleiding <= quants_gem_opl[5] ~ 4))
## Wijken
wijken <- wijken %>% 
  mutate(perc_opleiding = OpleidingsniveauLaag_64/AANT_INW*100)

quants_wk_opl <- quantile(wijken$perc_opleiding, na.rm = T)

wijken <- wijken %>% 
  mutate(opleidingsgroep = case_when(perc_opleiding >= quants_wk_opl[1] & 
                                       perc_opleiding < quants_wk_opl[2] ~ 1,
                                     perc_opleiding >= quants_wk_opl[2] & 
                                       perc_opleiding < quants_wk_opl[3] ~ 2,
                                     perc_opleiding >= quants_wk_opl[3] & 
                                       perc_opleiding < quants_wk_opl[4] ~ 3,
                                     perc_opleiding >= quants_wk_opl[4] & 
                                       perc_opleiding <= quants_wk_opl[5] ~ 4))

#Bind rows from gemeenten, wijken en buurten together 
gemeenten_test <- gemeenten  %>% add_column(Niveau = 'Gemeenten', CODE=gemeenten$GM_CODE, NAAM = gemeenten$GM_NAAM, .before = 'H2O')
wijken_test <- wijken %>% add_column(Niveau = 'Wijken', CODE = wijken$WK_CODE, NAAM = wijken$WK_NAAM,  .before = 'H2O') 
buurten_test <- buurten %>% add_column(Niveau = 'Buurten', CODE = buurten$BU_CODE, NAAM = buurten$BU_NAAM,  .before = 'H2O')

full_data <- bind_rows(buurten_test, wijken_test, gemeenten_test)
write_rds(full_data, "Data/full_data.rds")


