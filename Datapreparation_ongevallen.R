# Load libraries
library(dplyr)
library(sf)
library(patchwork)
library(tidyverse)
library(leaflet)
library(compareDF)

# Load ongevallen data
ongevallen_csv <- read.csv("Data/Verkeertrend/Ongevallengegevens/ongevallen.txt")
puntlocaties <- read.csv("Data/Verkeertrend/Netwerkgegevens/puntlocaties.txt")
afloop <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/aflopen3.txt")
aard <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/aardongevallen.txt")
partijen <- read.csv("Data/Verkeertrend/Ongevallengegevens/partijen.txt")
bewegingen <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/bewegingen.txt")
objecttypes <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/objecttypes.txt")
toedrachten <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/toedrachten.txt")
wegsituaties <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/wegsituaties.txt")
wegverharding <- read.csv("Data/Verkeertrend/ReferentiebestandenOngevallen/wegverhardingen.txt")
full_data <- readRDS("Data/full_data.rds")

ongevallen_joined <- ongevallen_csv %>% 
  left_join(puntlocaties, by = "FK_VELD5") %>% 
  left_join(afloop, by = "AP3_CODE") %>%   
  left_join(aard, by = "AOL_ID") %>% 
  left_join(partijen, by = "VKL_NUMMER") %>% 
  left_join(bewegingen, by = c("BWG_ID_1" = "BWG_ID")) %>% 
  left_join(objecttypes, by = "OTE_ID") %>% 
  left_join(toedrachten, by = c("TDT_ID_1" = "TDT_ID")) %>% 
  left_join(wegsituaties, by = "WSE_ID") %>% 
  left_join(wegverharding, by = "WVG_ID")

# Reduce dataset to what is important for further analysis
ongevallen_reduced <- ongevallen_joined %>% 
  select(VKL_NUMMER, JAAR_VKL, ANTL_PTJ, MAXSNELHD, WVG_ID, WGD_CODE_1, FK_VELD5, GME_ID, 
         GME_NAAM, PVE_NAAM, X_COORD, Y_COORD, AP3_OMS,AOL_OMS, UITGPOS1, 
         VOORGBEW, BWG_OMS, OTE_OMS, TDT_OMS, WSE_OMS, BZD_ID_VM1, BZD_ID_IF1) 

# Manually recode values of variables that have no reference file
ongevallen_reduced <- ongevallen_reduced %>% 
  mutate(UITGPOS1 = recode(UITGPOS1, "1" = "Rijbaan",
                           "2" = "Fietspad of fietsstrook",
                           "3" = "Trottoir of berm",
                           "4" = "Vluchtheuvel of middenberm",
                           "5" = "Inrit of uitrit",
                           "6" = "Vluchtstrook",
                           "7" = "Parkeervoorziening",
                           "8" = "Trambaan of busbaan")) %>% 
  mutate(VOORGBEW = recode(VOORGBEW, "1" = "Oversteken",
                           "2" = "Vooruit",
                           "3" = "Links rijstrook wisselen",
                           "4" = "Stilstand",
                           "5" = "Rechts rijstrook wisselen",
                           "6" = "Linksaf",
                           "7" = "Links omkeren",
                           "8" = "Achteruit",
                           "9" = "Rechts omkeren",
                           "10" = "Rechtsaf",
                           "11" = "Parkeerstand")) %>% 
  mutate(BZD_ID_VM1 = recode(BZD_ID_VM1, "100" = "Eenrichtingsstraat",
                             "110" = "VOP",
                             "120" = "Andere oversteekplaats",
                             "130" = "Voorsorteervakken",
                             "140" = "Voorrangskruispunt of voorrangsweg",
                             "150" = "Inhaalverbod",
                             "160" = "VRI werkend",
                             "170" = "VRI knipperend",
                             "180" = "VRI niet werkend")) %>% 
  mutate(BZD_ID_IF1 = recode(BZD_ID_IF1, "200" = "Brug of viaduct",
                             "210" = "Drempel of plateau",
                             "220" = "Tankstation",
                             "230" = "Weefvak",
                             "240" = "Ventweg of parallelweg",
                             "250" = "Bushalte of tramhalte",
                             "260" = "Parkeervoorziening",
                             "270" = "Spoorwegovergan",
                             "280" = "Tunnel",
                             "290" = "Versmalling")) %>% 
  mutate(WGD_CODE_1 = recode(WGD_CODE_1, "D" = "Droog",
                             "R" = "Regen",
                             "M" = "Mist",
                             "S" = "Sneeuw of Hagel",
                             "H" = "Harde windstoten",
                             "O" = "Onbekend"))

# Remove duplicates to have the same number of rows as original data
ongevallen_reduced_no_dup <- ongevallen_reduced %>% 
  distinct(VKL_NUMMER, .keep_all = T)

# Create correct geometry in New Amersfoort CRS and transform to W84
ongevallen_sf <- st_as_sf(ongevallen_reduced_no_dup, coords = c("X_COORD", "Y_COORD"), crs = "EPSG:28992")
ongevallen_transformed <- ongevallen_sf %>% st_transform("EPSG:4326")

write_rds(ongevallen_transformed, "Data/ongevallen_W84.rds")

# Full_data used from main branche
all_polygons <- full_data %>% 
  select(BU_CODE, BU_NAAM, WK_CODE, WK_NAAM, GM_CODE, GM_NAAM, POSTCODE, geometry, centroid, Niveau, CODE, NAAM, centroidx, centroidy)

# Already calculate all intersections of points an polygons
intersection <- st_intersection(x = all_polygons, y = ongevallen_transformed)

write_rds(intersection, "Data/intersection.rds")
