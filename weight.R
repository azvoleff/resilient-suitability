library(raster)
library(ggplot2)
library(dplyr)
library(plotly)
library(rgdal)

# Load a regional layer so that you can summarize by region

################################################################################
# Perform final overlay
################################################################################

norm_layer <- function(x) {
    x <- (x - cellStats(x, 'min'))
    x <- x / cellStats(x, 'max')
}

save_suitability <- function(x, name) {
    writeRaster(x, filename=paste0(name, '.tif'), overwrite=TRUE)
    v <- rasterToPolygons(x)
    writeOGR(v, paste0(name, '.kml'), 'layer', driver='KML', overwrite=TRUE)
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

calc_suitability <- function(model, pop_w, road_w, health_w) {
    stopifnot(model %in% c(1:3))
    if (model == 1) {
        s <- yield_gap * (yield_diff_cc > -5)
        name <- paste0('suitability_model_', model)
    } else if (model == 2) {
        s <- yield_gap * (yield_diff_cc > -5) * (1 - fc50) * (1 - pas)
        name <- paste0('suitability_model_', model)
    } else if (model == 3) {
        s <- yield_gap * (yield_diff_cc > -5) * (1 - fc50) * (1 - pas) *
               norm_layer(road_w * road_density + pop_w * pop_density + health_w * calc_health())
        name <- paste0('suitability_model_', model, '_p', pop_w, '_r', road_w, '_h', health_w)
    }
    s <- norm_layer(s)
    save_suitability(s, name)
    return(s)
}

plot(calc_suitability(1))
plot(calc_suitability(2))
plot(calc_suitability(3, pop_w=1, road_w=1, health_w=1))
plot(calc_suitability(3, pop_w=1, road_w=1, health_w=0))
plot(calc_suitability(3, pop_w=0, road_w=0, health_w=1))
