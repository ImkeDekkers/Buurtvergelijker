options( scipen = 999 )  #to get rid of the scientific notation in the tables
#Loading libraries
library(dplyr)
library(sf)
library("readxl")
#devtools::install_github('rstudio/leaflet')
library(leaflet)
library(tidyverse)
library('rmapshaper')

full_data <- readRDS("Data/full_data.rds")
full_data <- subset(full_data, select = -c(`Afstand tot huisartsenpraktijk (km)`:`Aantal musea binnen 20 km`))

#Reading RIVM data about health 2020
#18 and older
gezondheid_2020_18_eo <- read_xlsx("Data/RIVM_Gezondheid2020/RIVM_gezondheid2020_18+.xlsx")
gezondheid_2020_18_eo[, c(9:43)] <- sapply(gezondheid_2020_18_eo[, c(9:43)], as.numeric)
gezondheid_2020_18_eo$Leeftijd <- "18+"
gezondheid_2020_18_eo$Perioden <- "2020"
#18-65
gezondheid_2020_18_65 <- read_xlsx("Data/RIVM_Gezondheid2020/RIVM_gezondheid2020_18_65.xlsx")
gezondheid_2020_18_65[, c(9:43)] <- sapply(gezondheid_2020_18_65[, c(9:43)], as.numeric)
gezondheid_2020_18_65$Leeftijd <- "18-65"
gezondheid_2020_18_65$Perioden <- "2020"
#65 and older
gezondheid_2020_65_eo <- read_xlsx("Data/RIVM_Gezondheid2020/RIVM_gezondheid2020_65+.xlsx")
gezondheid_2020_65_eo[, c(9:43)] <- sapply(gezondheid_2020_65_eo[, c(9:43)], as.numeric)
gezondheid_2020_65_eo$Leeftijd <- "65+"
gezondheid_2020_65_eo$Perioden <- "2020"

#Reading RIVM data about health 2016
#18 and older
gezondheid_2016_18_eo <- read_xlsx("Data/RIVM_Gezondheid2016/2016_18+.xlsx")
gezondheid_2016_18_eo[, c(9:43)] <- sapply(gezondheid_2016_18_eo[, c(9:43)], as.numeric)
gezondheid_2016_18_eo$Leeftijd <- "18+"
gezondheid_2016_18_eo$Perioden <- "2016"
#18-65
gezondheid_2016_18_65 <- read_xlsx("Data/RIVM_Gezondheid2016/2016_18_65.xlsx")
gezondheid_2016_18_65[, c(9:43)] <- sapply(gezondheid_2016_18_65[, c(9:43)], as.numeric)
gezondheid_2016_18_65$Leeftijd <- "18-65"
gezondheid_2016_18_65$Perioden <- "2016"
#65 and older
gezondheid_2016_65_eo <- read_xlsx("Data/RIVM_Gezondheid2016/2016_65+.xlsx")
gezondheid_2016_65_eo[, c(9:43)] <- sapply(gezondheid_2016_65_eo[, c(9:43)], as.numeric)
gezondheid_2016_65_eo$Leeftijd <- "65+"
gezondheid_2016_65_eo$Perioden <- "2016"

#Reading RIVM data about health 2012
#18 and older
gezondheid_2012_18_eo <- read_xlsx("Data/RIVM_Gezondheid2012/2012_18+.xlsx")
gezondheid_2012_18_eo[, c(9:43)] <- sapply(gezondheid_2012_18_eo[, c(9:43)], as.numeric)
gezondheid_2012_18_eo$Leeftijd <- "18+"
gezondheid_2012_18_eo$Perioden <- "2012"
#18-65
gezondheid_2012_18_65 <- read_xlsx("Data/RIVM_Gezondheid2012/2012_18_65.xlsx")
gezondheid_2012_18_65[, c(9:43)] <- sapply(gezondheid_2012_18_65[, c(9:43)], as.numeric)
gezondheid_2012_18_65$Leeftijd <- "18-65"
gezondheid_2012_18_65$Perioden <- "2012"
#65 and older
gezondheid_2012_65_eo <- read_xlsx("Data/RIVM_Gezondheid2012/2012_65+.xlsx")
gezondheid_2012_65_eo[, c(9:43)] <- sapply(gezondheid_2012_65_eo[, c(9:43)], as.numeric)
gezondheid_2012_65_eo$Leeftijd <- "65+"
gezondheid_2012_65_eo$Perioden <- "2012"


#All data together (all years and ages)
gezondheid_all <- rbind(gezondheid_2012_18_eo, gezondheid_2012_18_65, gezondheid_2012_65_eo,
                        gezondheid_2016_18_eo, gezondheid_2016_18_65, gezondheid_2016_65_eo,
                        gezondheid_2020_18_eo, gezondheid_2020_18_65, gezondheid_2020_65_eo)

#Renaming variables
gezondheid_all <- rename(gezondheid_all,c("Zeer goede of goede gezondheid (%)" = ErvarenGezondheidGoedZeerGoed_4,
                                          "Voldoet aan beweegrichtlijn (%)" = VoldoetAanBeweegrichtlijn_5,
                                          "Wekelijkse sporters (%)" = WekelijkseSporters_6,
                                          "Ondergewicht (%)" = Ondergewicht_7,
                                          "Normaal gewicht (%)" = NormaalGewicht_8,
                                          "Overgewicht (%)" = Overgewicht_9,
                                          "Ernstig overgewicht (%)" = ErnstigOvergewicht_10,
                                          "Rokers (%)" = Roker_11,
                                          "Voldoet aan alcoholrichtlijn (%)" = VoldoetAanAlcoholRichtlijn_12,
                                          "Drinkers (%)" = Drinker_13,
                                          "Zware drinkers (%)" = ZwareDrinker_14,
                                          "Overmatige drinkers (%)" = OvermatigeDrinker_15,
                                          "Langdurige aandoening (%)" = EenOfMeerLangdurigeAandoeningen_16,
                                          "Beperkt vanwege gezondheid (%)" = BeperktVanwegeGezondheid_17,
                                          "Ernstig beperkt vanwege gezondheid (%)" = ErnstigBeperktVanwegeGezondheid_18,
                                          "Langdurige ziekte en beperkt (%)" = LangdurigeZiekteEnBeperkt_19,
                                          "Lichamelijke beperking (%)" = EenOfMeerLichamelijkeBeperkingen_20,
                                          "Beperking in horen (%)" = BeperkingInHoren_21,
                                          "Beperking in zien (%)" = BeperkingInZien_22,
                                          "Beperking in bewegen (%)" = BeperkingInBewegen_23,
                                          "Matig tot hoog risico op angststoornis of depressie (%)" = MatigHoogRisicoOpAngstOfDepressie_24,
                                          "Hoog risico op angststoornis of depressie (%)" = HoogRisicoOpAngstOfDepressie_25,
                                          "(heel) veel stress (%)" = HeelVeelStressInAfgelopen4Weken_26,
                                          "Matig tot veel regie over eigen leven (%)" = MatigVeelRegieOverEigenLeven_27,
                                          "Eenzaam (%)" = Eenzaam_28,
                                          "Ernstig eenzaam (%)" = ErnstigZeerErnstigEenzaam_29,
                                          "Emotioneel eenzaam (%)" = EmotioneelEenzaam_30,
                                          "Sociaal eenzaam (%)" = SociaalEenzaam_31,
                                          "Mantelzorger (%)" = Mantelzorger_32,
                                          "Vrijwilligerswerk (%)" = Vrijwilligerswerk_33,
                                          "Lopen en of fietsen naar school of werk (%)" = LopenEnOfFietsenNaarSchoolOfWerk_34,
                                          "Lopen naar school of werk (%)" = LopenNaarSchoolOfWerk_35,
                                          "Fietsen naar school of werk (%)" = FietsenNaarSchoolOfWerk_36,
                                          "Ernstige geluidhinder door buren (%)" = ErnstigeGeluidhinderDoorBuren_37,
                                          "Moeite met rondkomen (%)" = MoeiteMetRondkomen_38))


#Merging kerncijfers and gezondheid
gezondheid_all <- left_join(gezondheid_all, full_data, by = c("WijkenEnBuurten"="CODE"),suffix = c("", ""))
gezondheid_all <- st_as_sf(gezondheid_all)
gezondheid_all <- rename(gezondheid_all, "CODE"=Codering_3)


#Reading Kerncijfers gemeenten, wijken, buurten 2020 (to get the age distribution of the areas)
kerncijfers_gemeenten2020 <- st_read("../Data/WijkBuurtkaart_2020_v2/gemeente_2020_v2.shp")
kerncijfers_wijken2020 <- st_read("../Data/WijkBuurtkaart_2020_v2/wijk_2020_v2.shp")
kerncijfers_buurten2020 <- st_read("../Data/WijkBuurtkaart_2020_v2/buurt_2020_v2.shp")
#Cleaning Kerncijfers data
kerncijfers_gemeenten2020 <- kerncijfers_gemeenten2020[kerncijfers_gemeenten2020$H2O == "NEE", ]  
kerncijfers_wijken2020 <- kerncijfers_wijken2020[kerncijfers_wijken2020$H2O == "NEE", ]  
kerncijfers_buurten2020 <- kerncijfers_buurten2020[kerncijfers_buurten2020$H2O == "NEE", ]
#Change missing values from -99999999 to NA
gemeenten2020 <- kerncijfers_gemeenten2020 %>%
  mutate(across(where(is.numeric), ~na_if(.,-99999999)))
wijken2020 <- kerncijfers_wijken2020 %>%
  mutate(across(where(is.numeric), ~na_if(.,-99999999)))
buurten2020 <- kerncijfers_buurten2020 %>%
  mutate(across(where(is.numeric), ~na_if(.,-99999999)))
gemeenten2020 <- as.data.frame(gemeenten2020)
wijken2020 <- as.data.frame(wijken2020)
buurten2020 <- as.data.frame(buurten2020)
#Keep only age variables
gemeenten2020 <- subset(gemeenten2020, select = c(GM_CODE, P_00_14_JR, P_15_24_JR, P_25_44_JR, P_45_64_JR, P_65_EO_JR))
wijken2020 <- subset(wijken2020, select = c(WK_CODE, P_00_14_JR, P_15_24_JR, P_25_44_JR, P_45_64_JR, P_65_EO_JR))
buurten2020 <- subset(buurten2020, select = c(BU_CODE, P_00_14_JR, P_15_24_JR, P_25_44_JR, P_45_64_JR, P_65_EO_JR))

gemeente_gezondheid_all <- gezondheid_all[gezondheid_all$SoortRegio_2=="Gemeente",]
wijk_gezondheid_all <- gezondheid_all[gezondheid_all$SoortRegio_2=="Wijk",]
buurt_gezondheid_all <- gezondheid_all[gezondheid_all$SoortRegio_2=="Buurt",]

gemeente_gezondheid_all <- left_join(gemeente_gezondheid_all, gemeenten2020, by = c("CODE"="GM_CODE"),suffix = c("", ""))
wijk_gezondheid_all <- left_join(wijk_gezondheid_all, wijken2020, by = c("CODE"="WK_CODE"),suffix = c("", ""))
buurt_gezondheid_all <- left_join(buurt_gezondheid_all, buurten2020, by = c("CODE"="BU_CODE"),suffix = c("", ""))

#All data together
gezondheid_all <- rbind(gemeente_gezondheid_all, wijk_gezondheid_all, buurt_gezondheid_all)

gezondheid_all <- rename(gezondheid_all,c("Personen 0 tot 15 jaar (%)" = P_00_14_JR,
                                          "Personen 15 tot 25 jaar (%)" = P_15_24_JR,
                                          "Personen 25 tot 45 jaar (%)" = P_25_44_JR,
                                          "Personen 45 tot 65 jaar (%)" = P_45_64_JR,
                                          "Personen 65 jaar en ouder (%)" = P_65_EO_JR,))

write_rds(gezondheid_all, "Data/gezondheid_all.rds")
