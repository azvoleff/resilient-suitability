library(raster)
library(ggplot2)
library(dplyr)
library(plotly)

# Load a regional layer so that you can summarize by region

################################################################################
# Perform final overlay
################################################################################

norm_layer <- function(x) {
    x <- (x - cellStats(x, 'min'))
    x <- x / cellStats(x, 'max')
}

# TODO: Add in current potential yields

# Yield gap as difference between potential and actual production
yield_gap <- norm_layer(raster('AGRA_TZA_ygap_diff.tif'))
yield_diff_cc <- raster('AGRA_TZA_acy_diff.tif')

pas <- raster('AGRA_TZA_wdpa.tif')

fc30 <- raster('AGRA_forest_2015_30.tif')
fc50 <- raster('AGRA_forest_2015_50.tif')

pop_density <- norm_layer(raster('AGRA_TZA_pop_density.tif'))
road_density <- norm_layer(raster('AGRA_TZA_groads_density.tif'))

stunting <- norm_layer(raster('AGRA_TZA_DHS_stunted.tif'))
underweight <- norm_layer(raster('AGRA_TZA_DHS_underweight.tif'))
wasted <- norm_layer(raster('AGRA_TZA_DHS_wasted.tif'))

plot(stack(yield_gap, yield_diff_cc))

plot(pop_density)
plot(road_density)

plot(raster('AGRA_TZA_groads_density.tif'))

# Calculate difference in agro-climatic yield with climate change, and mask out 
# areas that will see a decline in agro-climatic yield

calc_health <- function(stunting_w=1, underweight_w=1, wasted_w=1) {
    return(norm_layer(stunting_w * stunting + underweight_w * underweight + 
                      wasted_w * wasted))
}
plot(stack(stunting, underweight, wasted, calc_health()))

calc_suitability <- function(pop_w, road_w, health_w) {
    return(yield_diff_cc * (yield_diff_cc > -5) *
           norm_layer(road_w * road_density + pop_w * pop_density + health_w * calc_health()))
}

