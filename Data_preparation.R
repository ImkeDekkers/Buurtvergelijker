options( scipen = 999 )  #to get rid of the scientific notation in the tables
#Loading libraries
library(dplyr)
library(sf)
library("readxl")
#devtools::install_github('rstudio/leaflet')
library(leaflet)
library('rmapshaper')

#Reading Kerncijfers gemeenten, wijken, buurten 2020
kerncijfers_gemeenten2020 <- st_read("C:/Users/ImkeDekkers/OneDrive - Universiteit Utrecht/Applied Data Science/Thesis/Data/WijkBuurtkaart_2020_v2/gemeente_2020_v2.shp")
kerncijfers_wijken2020 <- st_read("C:/Users/ImkeDekkers/OneDrive - Universiteit Utrecht/Applied Data Science/Thesis/Data/WijkBuurtkaart_2020_v2/wijk_2020_v2.shp")
kerncijfers_buurten2020 <- st_read("C:/Users/ImkeDekkers/OneDrive - Universiteit Utrecht/Applied Data Science/Thesis/Data/WijkBuurtkaart_2020_v2/buurt_2020_v2.shp")

#Cleaning Kerncijfers data
kerncijfers_gemeenten2020 <- kerncijfers_gemeenten2020[kerncijfers_gemeenten2020$H2O == "NEE", ]  
kerncijfers_wijken2020 <- kerncijfers_wijken2020[kerncijfers_wijken2020$H2O == "NEE", ]  
kerncijfers_buurten2020 <- kerncijfers_buurten2020[kerncijfers_buurten2020$H2O == "NEE", ]  

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

###Why are all geboorte en sterfte data 0??? WOZ is also all 0
