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
# Centroids gemeenten 
sf_use_s2(F)
gemeenten$centroid <- st_centroid(gemeenten$geometry)
gemeenten$centroidx <- NA
gemeenten$centroidy <- NA

for (x in seq(1: nrow(gemeenten))){
  gemeenten[x,]$centroidx <- st_coordinates(gemeenten$centroid)[x,1]
  gemeenten[x,]$centroidy <- st_coordinates(gemeenten$centroid)[x,2]
}

# Centroids wijken
wijken$centroid <- st_centroid(wijken$geometry)
wijken$centroidx <- NA
wijken$centroidy <- NA

for (x in seq(1: nrow(wijken))){
  wijken[x,]$centroidx <- st_coordinates(wijken$centroid)[x,1]
  wijken[x,]$centroidy <- st_coordinates(wijken$centroid)[x,2]
}

# Centroids buurten
buurten$centroid <- st_centroid(buurten$geometry)
buurten$centroidx <- NA
buurten$centroidy <- NA

for (x in seq(1: nrow(buurten))){
  buurten[x,]$centroidx <- st_coordinates(buurten$centroid)[x,1]
  buurten[x,]$centroidy <- st_coordinates(buurten$centroid)[x,2]
}

#Bind rows from gemeenten, wijken en buurten together 
gemeenten_test <- gemeenten  %>% add_column(Niveau = 'Gemeenten', CODE=gemeenten$GM_CODE, NAAM = gemeenten$GM_NAAM, .before = 'H2O')
wijken_test <- wijken %>% add_column(Niveau = 'Wijken', CODE = wijken$WK_CODE, NAAM = wijken$WK_NAAM,  .before = 'H2O') 
buurten_test <- buurten %>% add_column(Niveau = 'Buurten', CODE = buurten$BU_CODE, NAAM = buurten$BU_NAAM,  .before = 'H2O')

full_data <- bind_rows(buurten_test, wijken_test, gemeenten_test)
#full_data <- rename(full_data, c("Afstand tot cafÃ© (km)" = "Afstand tot cafe (km)", "Aantal cafÃ©s binnen 1 km" = "Aantal cafes binnen 1 km", "Aantal cafÃ©s binnen 3 km" = "Aantal cafes binnen 3 km", "Aantal cafÃ©s binnen 5 km" = "Aantal cafes binnen 5 km", "Aantal vestigingen financiÃ«le diensten, onroerend goed" = "Aantal vestigingen financiele diensten, onroerend goed") #, "Aantal personenautoâs rijdend op benzine" = "Aantal personenautos rijdend op benzine", "Aantal personenautoâs rijdend op overige brandstof" = "Aantal personenautos rijdend op overige brandstof" )
 
# names(full_data)[names(full_data) == "Aantal vestigingen financiÃ«le diensten, onroerend goed"] <- "Aantal vestigingen financiele diensten, onroerend goed"
# names(full_data)[names(full_data) == "Aantal personenautoâs rijdend op benzine"] <- "Aantal personenautos rijdend op benzine"
# names(full_data)[names(full_data) == "Aantal personenautoâs rijdend op overige brandstof"] <- "Aantal personenautos rijdend op overige brandstof"

#simplify full_data to load map faster
full_data <- rmapshaper::ms_simplify(full_data, keep = 0.05, keep_shapes = TRUE)

full_data <- full_data %>% 
  select(inkomengroep, centroidx, centroidy, BU_CODE, BU_NAAM, WK_CODE, WK_NAAM, GM_CODE, GM_NAAM, Niveau, CODE, NAAM, POSTCODE, centroid, geometry,
         STED, AF_ARTSPR, AV1_ARTSPR, AV3_ARTSPR, AV5_ARTSPR, AF_ARTSPO, AF_APOTH, AF_ZIEK_I, AV5_ZIEK_I, AV10ZIEK_I, AV20ZIEK_I,
         AF_ZIEK_E, AV5_ZIEK_E, AV10ZIEK_E, AV20ZIEK_E, AF_SUPERM, AV1_SUPERM, AV3_SUPERM, AV5_SUPERM, AF_DAGLMD, AV1_DAGLMD, AV3_DAGLMD, 
         AV5_DAGLMD, AF_WARENH, AV5_WARENH, AV10WARENH, AV20WARENH, AF_CAFE, AV1_CAFE, AV3_CAFE, AV5_CAFE, AF_CAFTAR, AV1_CAFTAR, AV3_CAFTAR,
         AV5_CAFTAR, AF_RESTAU, AV1_RESTAU, AV3_RESTAU, AV5_RESTAU, AF_HOTEL, AV5_HOTEL, AV10_HOTEL, AV20_HOTEL, AF_KDV, AV1_KDV, AV3_KDV,
         AV5_KDV, AF_BSO, AV1_BSO, AV3_BSO, AV5_BSO, AF_ONDBAS, AV1_ONDBAS, AV3_ONDBAS, AV5_ONDBAS, AF_ONDVRT, AV3_ONDVRT, AV5_ONDVRT, AV10ONDVRT,
         AF_ONDVMB, AV3_ONDVMB, AV5_ONDVMB, AV10ONDVMB, AF_ONDHV, AV3_ONDHV, AV5_ONDHV, AV10_ONDHV, AF_OPRITH, AF_TREINST, AF_OVERST, AF_ZWEMB,
         AF_IJSBAAN, AF_BIBLIO, AF_POP, AF_BIOS, AV5_BIOS, AV10_BIOS, AV20_BIOS, AF_SAUNA, AF_ZONBNK, AF_ATTRAC, AV10ATTRAC, AV20ATTRAC, AV50ATTRAC,
         AF_PODIUM, AV5_PODIUM, AV10PODIUM, AV20PODIUM, AF_MUSEUM, AV5_MUSEUM, AV10MUSEUM, AV20MUSEUM)

full_data <- rename(full_data, c("Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)" = STED,
                                 "Afstand tot huisartsenpraktijk (km)" = AF_ARTSPR,
                                 "Aantal huisartsenpraktijken binnen 1 km" = AV1_ARTSPR,
                                 "Aantal huisartsenpraktijken binnen 3 km" = AV3_ARTSPR,
                                 "Aantal huisartsenpraktijken binnen 5 km" = AV5_ARTSPR,
                                 "Afstand tot huisartsenpost (km)" = AF_ARTSPO,
                                 "Afstand tot apotheek (km)" = AF_APOTH,
                                 "Afstand tot ziekenhuis incl. buitenpolikliniek (km)" = AF_ZIEK_I,
                                 "Aantal ziekenhuizen incl. buitenpolikliniek binnen 5 km" = AV5_ZIEK_I,
                                 "Aantal ziekenhuizen incl. buitenpolikliniek binnen 10 km" = AV10ZIEK_I,
                                 "Aantal ziekenhuizen incl. buitenpolikliniek binnen 20 km" = AV20ZIEK_I,
                                 "Afstand tot ziekenhuis excl. Buitenpolikliniek (km)" =  AF_ZIEK_E,
                                 "Aantal ziekenhuizen excl. Buitenpolikliniek binnen 5 km" = AV5_ZIEK_E,
                                 "Aantal ziekenhuizen excl. Buitenpolikliniek binnen 10 km" = AV10ZIEK_E,
                                 "Aantal ziekenhuizen excl. Buitenpolikliniek binnen 20 km" = AV20ZIEK_E,
                                 "Afstand tot grote supermarkt (km)" = AF_SUPERM,
                                 "Aantal  grote supermarkten binnen 1 km" = AV1_SUPERM,
                                 "Aantal  grote supermarkten binnen 3 km" = AV3_SUPERM,
                                 "Aantal  grote supermarkten binnen 5 km" = AV5_SUPERM,
                                 "Afstand tot overige dagelijkse levensmiddelen (km)" = AF_DAGLMD,
                                 "Aantal winkels overige dagelijkse levensmiddelen binnen 1 km" = AV1_DAGLMD,
                                 "Aantal winkels overige dagelijkse levensmiddelen binnen 3 km" = AV3_DAGLMD,
                                 "Aantal winkels overige dagelijkse levensmiddelen binnen 5 km" = AV5_DAGLMD,
                                 "Afstand tot warenhuis (km)"  = AF_WARENH,
                                 "Aantal warenhuizen binnen 5 km" = AV5_WARENH,
                                 "Aantal warenhuizen binnen 10 km" = AV10WARENH,
                                 "Aantal warenhuizen binnen 20 km" = AV20WARENH,
                                 "Afstand tot cafe (km)" = AF_CAFE,
                                 "Aantal cafes binnen 1 km" = AV1_CAFE,
                                 "Aantal cafes binnen 3 km" = AV3_CAFE,
                                 "Aantal cafes binnen 5 km" = AV5_CAFE,
                                 "Afstand tot cafetaria (km)" = AF_CAFTAR,
                                 "Aantal cafetaria's binnen 1 km" = AV1_CAFTAR,
                                 "Aantal cafetaria's binnen 3 km" = AV3_CAFTAR,
                                 "Aantal cafetaria's binnen 5 km" = AV5_CAFTAR,
                                 "Afstand tot restaurant (km)" = AF_RESTAU,
                                 "Aantal restaurants binnen 1 km" = AV1_RESTAU,
                                 "Aantal restaurants binnen 3 km" = AV3_RESTAU,
                                 "Aantal restaurants binnen 5 km" = AV5_RESTAU,
                                 "Afstand tot hotel (km)" = AF_HOTEL,
                                 "Aantal hotel binnen 5 km" = AV5_HOTEL,
                                 "Aantal hotel binnen 10 km" = AV10_HOTEL,
                                 "Aantal hotel binnen 20 km" = AV20_HOTEL,
                                 "Afstand tot kinderdagverblijf  (km)" = AF_KDV,
                                 "Aantal kinderdagverblijf  binnen 1 km" = AV1_KDV,
                                 "Aantal kinderdagverblijf  binnen 3 km" = AV3_KDV,
                                 "Aantal kinderdagverblijf  binnen 5 km" = AV5_KDV,
                                 "Afstand tot buitenschoolse opvang  (km)" = AF_BSO,
                                 "Aantal buitenschoolse opvang  binnen 1 km" = AV1_BSO,
                                 "Aantal buitenschoolse opvang  binnen 3 km" = AV3_BSO,
                                 "Aantal buitenschoolse opvang  binnen 5 km" = AV5_BSO,
                                 "Afstand tot basisscholen (km)" = AF_ONDBAS,
                                 "Aantal basisscholen binnen 1 km" = AV1_ONDBAS,
                                 "Aantal basisscholen binnen 3 km" = AV3_ONDBAS,
                                 "Aantal basisscholen binnen 5 km" = AV5_ONDBAS,
                                 "Afstand tot voortgezet onderwijs (km)" = AF_ONDVRT,
                                 "Aantal voortgezet onderwijs binnen 3 km" = AV3_ONDVRT,
                                 "Aantal voortgezet onderwijs binnen 5 km" = AV5_ONDVRT,
                                 "Aantal voortgezet onderwijs binnen 10 km" = AV10ONDVRT,
                                 "Afstand tot scholen VMBO (km)" = AF_ONDVMB,
                                 "Aantal scholen VMBO binnen 3 km" = AV3_ONDVMB,
                                 "Aantal scholen VMBO binnen 5 km" = AV5_ONDVMB,
                                 "Aantal scholen VMBO binnen 10 km" = AV10ONDVMB,
                                 "Afstand tot scholen HAVO/VWO (km)" = AF_ONDHV,
                                 "Aantal scholen HAVO/VWO binnen 3 km" = AV3_ONDHV,
                                 "Aantal scholen HAVO/VWO binnen 5 km" = AV5_ONDHV,
                                 "Aantal scholen HAVO/VWO binnen 10 km" = AV10_ONDHV,
                                 "Afstand tot oprit hoofdverkeersweg (km)" = AF_OPRITH,
                                 "Afstand tot treinstation (km)" = AF_TREINST,
                                 "Afstand tot belangrijk overstapstation (km)" = AF_OVERST,
                                 "Afstand tot zwembad (km)" = AF_ZWEMB,
                                 "Afstand tot kunstijsbaan (km)" = AF_IJSBAAN,
                                 "Afstand tot bibliotheek (km)" = AF_BIBLIO,
                                 "Afstand tot poppodium (km)" = AF_POP,
                                 "Afstand tot bioscoop (km)" = AF_BIOS,
                                 "Aantal bioscoop binnen 5 km" = AV5_BIOS,
                                 "Aantal bioscoop binnen 10 km" = AV10_BIOS,
                                 "Aantal bioscoop binnen 20 km" = AV20_BIOS,
                                 "Afstand tot sauna (km)" = AF_SAUNA,
                                 "Afstand tot zonnebank (km)" = AF_ZONBNK,
                                 "Afstand tot attractie (km)" = AF_ATTRAC,
                                 "Aantal attracties binnen 10 km" = AV10ATTRAC,
                                 "Aantal attracties binnen 20 km" = AV20ATTRAC,
                                 "Aantal attracties binnen 50 km" = AV50ATTRAC,
                                 "Afstand tot podiumkunsten (km)" = AF_PODIUM,
                                 "Aantal podiumkunsten binnen 5 km" = AV5_PODIUM,
                                 "Aantal podiumkunsten binnen 10 km" = AV10PODIUM,
                                 "Aantal podiumkunsten binnen 20 km" = AV20PODIUM,
                                 "Afstand tot museum (km)" = AF_MUSEUM,
                                 "Aantal musea binnen 5 km" = AV5_MUSEUM,
                                 "Aantal musea binnen 10 km" = AV10MUSEUM,
                                 "Aantal musea binnen 20 km" = AV20MUSEUM))

names(full_data)[names(full_data) == "Afstand tot café (km)"] <- "Afstand tot cafe (km)"
names(full_data)[names(full_data) == "Aantal cafés binnen 1 km"] <- "Aantal cafes binnen 1 km"
names(full_data)[names(full_data) == "Aantal cafés binnen 3 km"] <- "Aantal cafes binnen 3 km"
names(full_data)[names(full_data) == "Aantal cafés binnen 5 km"] <- "Aantal cafes binnen 5 km"

write_rds(full_data, "Data/full_data.rds")


