library(dplyr)
library(sf)
library(patchwork)
library(tidyverse)
library(leaflet)
library(compareDF)

# Load ongevallen data
ongevallen_csv <- read.csv("../Data/Verkeertrend/Ongevallengegevens/ongevallen.txt")
puntlocaties <- read.csv("../Data/Verkeertrend/Netwerkgegevens/puntlocaties.txt")
afloop <- read.csv("../Data/Verkeertrend/ReferentiebestandenOngevallen/aflopen3.txt")
aard <- read.csv("../Data/Verkeertrend/ReferentiebestandenOngevallen/aardongevallen.txt")
wegvakken <- read.csv("../Data/Verkeertrend/Netwerkgegevens/wegvakken.txt")                # Wel missings 

# bijzonderheden <- read.csv("C:/Users/pasca/Local ADS thesis/Data/Verkeertrend/ReferentiebestandenOngevallen/bijzonderheden.txt")
# bewegingen <- read.csv("C:/Users/pasca/Local ADS thesis/Data/Verkeertrend/ReferentiebestandenOngevallen/bewegingen.txt")
# objecttypes <- read.csv("C:/Users/pasca/Local ADS thesis/Data/Verkeertrend/ReferentiebestandenOngevallen/objecttypes.txt")

ongevallen_joined <- ongevallen_csv %>% 
  left_join(puntlocaties) %>% 
  left_join(afloop) %>%   
  left_join(aard) %>% 
  left_join(wegvakken) 

# Create correct geometry in New Amersfoort CRS and transform to W84
ongevallen_sf <- st_as_sf(ongevallen_joined, coords = c("X_COORD", "Y_COORD"), crs = "EPSG:28992")
ongevallen_transformed <- ongevallen_sf %>% st_transform("EPSG:4326")

write_rds(ongevallen_transformed, "../Data/ongevallen_W84.rds")