library(raster)
library(ggplot2)
library(dplyr)

# Load a regional layer so that you can summarize by region

################################################################################
# Perform final overlay
################################################################################

norm_layer <- function(x) {
    x <- (x - cellStats(x, 'min')) / cellStats(x, 'max')
}

yield_gap <- norm_layer(raster('AGRA_TZA_ygap_diff.tif'))
yield_diff_cc <- norm_layer(raster('AGRA_TZA_acy_diff.tif'))

pop_density <- norm_layer(raster('AGRA_TZA_pop_density.tif'))
road_density <- norm_layer(raster('AGRA_TZA_groads_density.tif'))

stunting <- norm_layer(raster('AGRA_TZA_DHS_stunting.kml'))
underweight <- norm_layer(raster('AGRA_TZA_DHS_underweight.kml'))
wasted <- norm_layer(raster('AGRA_TZA_DHS_wasted.kml'))

# Calculate difference in agro-climatic yield with climate change, and mask out 
# areas that will see a decline in agro-climatic yield
acy_exclude <- acy_diff >= -5

# First go:
# potential <- mask(poss, get_country_poly())
# potential <- potential/cellStats(potential, 'max')
#
# potential_poly <- rasterToPolygons(poss)
# writeOGR(potential_poly, 'AGRA_TZA_output.kml', 'layer', driver='KML', 
#          overwrite=TRUE)

