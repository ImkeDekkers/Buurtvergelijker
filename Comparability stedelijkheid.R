# What to do
## Create groups for stedelijkheid so that it is possible to check if the gemeente/buurt/wijk is in that list
## Select an input postal code 
## Check what the stedelijkheid is and to which group it belongs
## Only display values of variable chosen for gemeente/wijk/buurt that has the same stedelijkheid


hist(gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`)

# Gemeenten
## Create groups for stedelijkheid 
gem_stedelijkheid1 <- gemeenten[gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== 1,]
gem_stedelijkheid2 <- gemeenten[gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== 2,]
gem_stedelijkheid3 <- gemeenten[gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== 3,]
gem_stedelijkheid4 <- gemeenten[gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== 4,]
gem_stedelijkheid5 <- gemeenten[gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== 5,]

gem_stedelijkheid1$GM_NAAM

# Wijken
## Create groups for stedelijkheid
wijk_stedelijkheid1 <- wijken[wijken$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==1,]
wijk_stedelijkheid2 <- wijken[wijken$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==2,]
wijk_stedelijkheid3 <- wijken[wijken$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==3,]
wijk_stedelijkheid4 <- wijken[wijken$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==4,]
wijk_stedelijkheid5 <- wijken[wijken$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==5,]

wijk_stedelijkheid1$WK_NAAM

# Buurten
## Create groups for stedelijkheid
buurt_stedelijkheid1 <- buurten[buurten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==1,]
buurt_stedelijkheid2 <- buurten[buurten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==2,]
buurt_stedelijkheid3 <- buurten[buurten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==3,]
buurt_stedelijkheid4 <- buurten[buurten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==4,]
buurt_stedelijkheid4 <- buurten[buurten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`==5,]

buurt_stedelijkheid1$BU_NAAM

