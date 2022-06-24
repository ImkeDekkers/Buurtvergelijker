# Buurtvergelijker
The R Shiny dashboard is designed to give insights into open data published by several Dutch governmental institutions. The dashboard contains four different tabs for different subjects, namely amenities, health, crime and traffic incidents. It is designed for Dutch citizens who want to gain more insights into their own neighborhood and to be able to compare their neighborhood to comparable neighborhoods. Comparable neighborhoods can be selected using the same degree of urbanization or income. 

## Included visualizations
- Leaflet maps
- Ggplot: bar chart, trend line, pie chart, histogram
- Top 5 most similar areas 

## How to get the R Shiny dashboard to work
The preprocessed data cannot be uploaded on GitHub because of its size. Therefore, some of the original dataset need to be retrieved from the supplier. After downloading the data, the data prep(aration) files can be run to prepare the data for use in the dashboard. 

After doing so, please make sure to store the data properly. The data-folder should be in the same folder as the Shiny_app.

Data: ../Buurtvergelijker/Data
Shiny_app: ../Buurtvergelijker/Shiny_app

## Needed packages
- shiny
- shinythemes
- shinydashboard
- shinyWidgets
- htmltools
- ggplot2
- ggrepel
- sf
- rmapshaper
- leaflet
- stringr
- tidyverse
- scales
- RColorBrewer
- readxl

## Original datasets 
* Amenities: https://opendata.cbs.nl/statline/#/CBS/nl/dataset/84953NED/table?ts=1654785777075
* Postal codes: https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/gegevens-per-postcode
* Traffic incidents: https://nationaalgeoregister.nl/geonetwork/srv/dut/catalog.search#/metadata/4gqrs90k-vobr-5t59-x726-4x2unrs1vawz?tab=relations
* Health: For the health tab, the unpreprocessed data is available on GitHub. There are three different folders of health data: RIVM_Gezondheid2012, RIVM_Gezondheid2016, RIVMGezondheid2020. These can be found in the Data folder.
* Crime: 
