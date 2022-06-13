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


#Reading RIVM data about health 2020
#18 and older
gezondheid_2020_18_eo <- read_xlsx("../Data/RIVM_Gezondheid2020/RIVM_gezondheid2020_18+.xlsx")
gezondheid_2020_18_eo[, c(9:43)] <- sapply(gezondheid_2020_18_eo[, c(9:43)], as.numeric)
gezondheid_2020_18_eo$Leeftijd <- "18+"
gezondheid_2020_18_eo$Perioden <- "2020"
#18-65
gezondheid_2020_18_65 <- read_xlsx("../Data/RIVM_Gezondheid2020/RIVM_gezondheid2020_18_65.xlsx")
gezondheid_2020_18_65[, c(9:43)] <- sapply(gezondheid_2020_18_65[, c(9:43)], as.numeric)
gezondheid_2020_18_65$Leeftijd <- "18-65"
gezondheid_2020_18_65$Perioden <- "2020"
#65 and older
gezondheid_2020_65_eo <- read_xlsx("../Data/RIVM_Gezondheid2020/RIVM_gezondheid2020_65+.xlsx")
gezondheid_2020_65_eo[, c(9:43)] <- sapply(gezondheid_2020_65_eo[, c(9:43)], as.numeric)
gezondheid_2020_65_eo$Leeftijd <- "65+"
gezondheid_2020_65_eo$Perioden <- "2020"

#Reading RIVM data about health 2016
#18 and older
gezondheid_2016_18_eo <- read_xlsx("../Data/RIVM_Gezondheid2016/2016_18+.xlsx")
gezondheid_2016_18_eo[, c(9:43)] <- sapply(gezondheid_2016_18_eo[, c(9:43)], as.numeric)
gezondheid_2016_18_eo$Leeftijd <- "18+"
gezondheid_2016_18_eo$Perioden <- "2016"
#18-65
gezondheid_2016_18_65 <- read_xlsx("../Data/RIVM_Gezondheid2016/2016_18_65.xlsx")
gezondheid_2016_18_65[, c(9:43)] <- sapply(gezondheid_2016_18_65[, c(9:43)], as.numeric)
gezondheid_2016_18_65$Leeftijd <- "18-65"
gezondheid_2016_18_65$Perioden <- "2016"
#65 and older
gezondheid_2016_65_eo <- read_xlsx("../Data/RIVM_Gezondheid2016/2016_65+.xlsx")
gezondheid_2016_65_eo[, c(9:43)] <- sapply(gezondheid_2016_65_eo[, c(9:43)], as.numeric)
gezondheid_2016_65_eo$Leeftijd <- "65+"
gezondheid_2016_65_eo$Perioden <- "2016"

#Reading RIVM data about health 2012
#18 and older
gezondheid_2012_18_eo <- read_xlsx("../Data/RIVM_Gezondheid2012/2012_18+.xlsx")
gezondheid_2012_18_eo[, c(9:43)] <- sapply(gezondheid_2012_18_eo[, c(9:43)], as.numeric)
gezondheid_2012_18_eo$Leeftijd <- "18+"
gezondheid_2012_18_eo$Perioden <- "2012"
#18-65
gezondheid_2012_18_65 <- read_xlsx("../Data/RIVM_Gezondheid2012/2012_18_65.xlsx")
gezondheid_2012_18_65[, c(9:43)] <- sapply(gezondheid_2012_18_65[, c(9:43)], as.numeric)
gezondheid_2012_18_65$Leeftijd <- "18-65"
gezondheid_2012_18_65$Perioden <- "2012"
#65 and older
gezondheid_2012_65_eo <- read_xlsx("../Data/RIVM_Gezondheid2012/2012_65+.xlsx")
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

#Removing unnecessary columns
gezondheid_all <- subset(gezondheid_all, select = -c(`Ongehuwd (%)`:`Mediaan vermogen van particuliere huishoudens (x1000 euro)`))


write_rds(gezondheid_all, "Data/gezondheid_all.rds")


