# What to do
## Select an input postal code 
## Check what the stedelijkheid is 
## Use the stedelijkheid number to create subset of comparable gemeenten/wijken/buurten
## Only display values of variable chosen for gemeente/wijk/buurt that has the same stedelijkheid

### We have input for gemeenten, wijken, buurten
### If one of these is selected, then it should be checked if the selected postal code is in the corresponding gem, wijk or buurt stedelijkheid variable
### The histogram should be based on the data of the selected gemeente, wijk or buurt with the same stedelijkheid

## WORK IN PROGRESS


# GEMEENTEN
# Reshape data to data frame (not with shape files)
df_gemeenten <- as.data.frame(gemeenten)

# Check stedelijkheid number for given gemeente
## (maybe update this to find the name of the column)
stedelijkheid_num_gem <- df_gemeenten[df_gemeenten$GM_NAAM=="Groningen", 5]               # Stedelijkheid is the 5th column in the data

# Create the right data based on the given stedelijkheid number
comparable_gemeenten <- gemeenten[gemeenten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num, ]     # Should be input for visualizations


# WIJKEN
# Reshape data to data frame (not with shape files)
df_wijken <- as.data.frame(wijken)

# Check stedelijkheid number for given wijk
## (maybe update this to find the name of the column)
stedelijkheid_num_wijken <- df_wijken[df_wijken$WK_NAAM=="Fivelzigt", 8]                  # Stedelijkheid is the 8th column in the data

# Create the right data based on the given stedelijkheid number
comparable_wijken <- wijken[wijken$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_wijken, ]      # Should be input for visualizations


# BUURTEN
# Reshape data to data frame (not with shape files)
df_buurten <- as.data.frame(buurten)

# Check stedelijkheid number for given gemeente
## (maybe update this to find the name of the column)
stedelijkheid_num_buurten <- df_buurten[df_buurten$BU_NAAM=="Appingedam-Centrum", 11]     # Stedelijkheid is the 11th column in the data

# Create the right data based on the given stedelijkheid number 
comparable_buurten <- buurten[buurten$`Stedelijkheid (1=zeer sterk stedelijk, 5=niet stedelijk)`== stedelijkheid_num_buurten, ]  # Should be input for visualizations


