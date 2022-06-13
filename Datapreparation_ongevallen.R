# Load libraries
library(dplyr)
library(sf)
library(tidyverse)

# Load ongevallen data
ongevallen_csv <- read.csv("Data/Verkeertrend/Ongevallengegevens/ongevallen.txt")
puntlocaties <- read.csv("Data/Verkeertrend/Netwerkgegevens/puntlocaties.txt")
afloop <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/aflopen3.txt")
aard <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/aardongevallen.txt")
partijen <- read.csv("Data/Verkeertrend/Ongevallengegevens/partijen.txt")
objecttypes <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/objecttypes.txt")
wegsituaties <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/wegsituaties.txt")
full_data <- readRDS("Data/full_data.rds")

ongevallen_joined <- ongevallen_csv %>% 
  left_join(puntlocaties, by = "FK_VELD5") %>% 
  left_join(afloop, by = "AP3_CODE") %>%   
  left_join(aard, by = "AOL_ID") %>% 
  left_join(partijen, by = "VKL_NUMMER") %>% 
  left_join(objecttypes, by = "OTE_ID") %>% 
  left_join(wegsituaties, by = "WSE_ID") 

# Reduce dataset to what is important for further analysis
ongevallen_reduced <- ongevallen_joined %>% 
  select(VKL_NUMMER, JAAR_VKL, ANTL_PTJ, MAXSNELHD, WGD_CODE_1, FK_VELD5, 
         X_COORD, Y_COORD, AP3_OMS, AOL_OMS, OTE_OMS, WSE_OMS) 

# Manually recode values of variable that has no reference file
ongevallen_reduced <- ongevallen_reduced %>% 
  mutate(WGD_CODE_1 = recode(WGD_CODE_1, "D" = "Droog",
                             "R" = "Regen",
                             "M" = "Mist",
                             "S" = "Sneeuw of Hagel",
                             "H" = "Harde windstoten",
                             "O" = "Onbekend")) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  mutate(OTE_OMS = recode(OTE_OMS, "e-bike" = "Gemotoriseerde fiets of scootmobiel",
                          "Fiets" = "Fiets",
                          "Motor" = "Motor",
                          "Bromfiets" = "Gemotoriseerde fiets of scootmobiel",
                          "Landbouwvoertuig" = "Landbouwvoertuig",
                          "Brommobiel" = "Gemotoriseerde fiets of scootmobiel",
                          "Boom" = "Voorwerp of object",
                          "Dier" = "Voorwerp of object",
                          "Bestelauto" = "Personen- of bestelauto",
                          "Scootmobiel" = "Gemotoriseerde fiets of scootmobiel",
                          "Vrachtauto" = "Vrachtauto of bus",
                          "Snorfiets" = "Gemotoriseerde fiets of scootmobiel",
                          "Voetganger" = "Voetganger",
                          "Onbekend voertuig i.g.v. doorrijder" = "Onbekend",
                          "Los voorwerp" = "Voorwerp of object",
                          "Personenauto" = "Personen- of bestelauto",
                          "Trekker" = "Landbouwvoertuig",
                          "Bus" = "Vrachtauto of bus",
                          "Overig vast object" = "Voorwerp of object",
                          "Trein/tram" = "Trein/tram",
                          "Trekker met oplegger" = "Landbouwvoertuig",
                          "Overig wegmeubilair" = "Voorwerp of object",
                          "Lichtmast" = "Voorwerp of object")) %>% 
  mutate(ANTL_PTJ = recode(ANTL_PTJ, "0" = "0",
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           .default = "5 of meer")) %>% 
  replace_na(list(OTE_OMS = "Onbekend", WSE_OMS = "Onbekend"))

# Remove duplicates to have the same number of rows as original data
ongevallen_reduced_no_dup <- ongevallen_reduced %>% 
  distinct(VKL_NUMMER, .keep_all = T)

# Create correct geometry in New Amersfoort CRS and transform to W84
ongevallen_sf <- st_as_sf(ongevallen_reduced_no_dup, coords = c("X_COORD", "Y_COORD"), crs = "EPSG:28992")
ongevallen_transformed <- ongevallen_sf %>% st_transform("EPSG:4326")

write_rds(ongevallen_transformed, "Data/ongevallen_W84.rds")

# Full_data used from main branche
all_polygons <- full_data %>% 
  select(BU_CODE, BU_NAAM, WK_CODE, WK_NAAM, GM_CODE, GM_NAAM,
         geometry, centroid, Niveau, centroidx, centroidy, `Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)

write_rds(all_polygons, "Data/all_polygons.rds")

# Already calculate all intersections of points an polygons
intersection <- st_intersection(x = all_polygons, y = ongevallen_transformed)

write_rds(intersection, "Data/intersection.rds")
