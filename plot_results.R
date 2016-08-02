library(httr)
library(dplyr)
library(plotly)
library(rgdal)
library(tools)

# Expand 

# Plotly plot of total yield for a range of thresholds


d <- readOGR('output.kml', 'output')


#d <- readOGR('AGRA_TZA_ygap_diff.kml')
