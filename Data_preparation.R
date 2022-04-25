options( scipen = 999 )  #to get rid of the scientific notation in the tables
#Loading libraries
library(dplyr)
library(sf)
library("readxl")
#devtools::install_github('rstudio/leaflet')
library(leaflet)
library('rmapshaper')

#Reading Kerncijfers gemeenten, wijken, buurten
kerncijfers_gemeenten <- st_read("C:/Users/ImkeDekkers/OneDrive - Universiteit Utrecht/Applied Data Science/Thesis/Data/WijkBuurtkaart_2021_v1/gemeente_2021_v1.shp")
kerncijfers_wijken <- st_read("C:/Users/ImkeDekkers/OneDrive - Universiteit Utrecht/Applied Data Science/Thesis/Data/WijkBuurtkaart_2021_v1/wijk_2021_v1.shp")
kerncijfers_buurten <- st_read("C:/Users/ImkeDekkers/OneDrive - Universiteit Utrecht/Applied Data Science/Thesis/Data/WijkBuurtkaart_2021_v1/buurt_2021_v1.shp")

#Cleaning Kerncijfers data
kerncijfers_gemeenten <- kerncijfers_gemeenten[kerncijfers_gemeenten$H2O == "NEE", ]  
kerncijfers_wijken <- kerncijfers_wijken[kerncijfers_wijken$H2O == "NEE", ]  
kerncijfers_buurten <- kerncijfers_buurten[kerncijfers_buurten$H2O == "NEE", ]  

#Read nabijheid voorzieningen data
voorzieningen <- read.csv("C:/Users/ImkeDekkers/OneDrive - Universiteit Utrecht/Applied Data Science/Thesis/Data/NabijheidVoorzieningen2020/NabijheidVoorzieningen.csv")
voorzieningen <- voorzieningen %>%  #changes columns 6 to 115 from characters to numeric 
  mutate_at(c(6:115), as.numeric)
voorzieningen <- voorzieningen[, which(colMeans(!is.na(voorzieningen)) > 0.01)] #Verwijdert columns met 99% of meer missing data (18 columns)

#divide voorzieningen df in three df's: gemeenten, wijken en buurten
gemeente_voorzieningen <- voorzieningen[ which(voorzieningen$SoortRegio_2=='Gemeente'), ]
wijken_voorzieningen <- voorzieningen[ which(voorzieningen$SoortRegio_2=='Wijk'), ]
buurten_voorzieningen <- voorzieningen[ which(voorzieningen$SoortRegio_2=='Buurt'), ]

#Merging voorzieningen and kerncijfers 
gemeenten <- merge(kerncijfers_gemeenten,gemeente_voorzieningen, by.x="GM_CODE", by.y="Codering_3",all.x=TRUE)
wijken <- merge(kerncijfers_wijken,wijken_voorzieningen, by.x="WK_CODE", by.y="Codering_3",all.x=TRUE)
buurten <- merge(kerncijfers_buurten,buurten_voorzieningen, by.x="BU_CODE", by.y="Codering_3",all.x=TRUE)

#Transforming gemeenten, wijken en buurten dataset to CRS 4326 (om leaflet functie werkend te krijgen)
gemeenten <- st_transform(gemeenten, "+init=epsg:4326")
wijken <- st_transform(wijken, "+init=epsg:4326")
buurten <- st_transform(buurten, "+init=epsg:4326")

#Change missing values from -99999999 to NA
gemeenten <- gemeenten %>%
  mutate(across(where(is.numeric), ~na_if(.,-99999999)))
wijken <- wijken %>%
  mutate(across(where(is.numeric), ~na_if(.,-99999999)))
buurten <- buurten %>%
  mutate(across(where(is.numeric), ~na_if(.,-99999999)))

#simplifying the geometries so loading the map goes faster
gemeenten <- rmapshaper::ms_simplify(gemeenten, keep = 0.05, keep_shapes = TRUE)
wijken <- rmapshaper::ms_simplify(wijken, keep = 0.05, keep_shapes = TRUE)
buurten <- rmapshaper::ms_simplify(buurten, keep = 0.05, keep_shapes = TRUE)
