library(raster)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggthemes)
library(rgdal)
library(grid)
library(gridExtra)
library(foreach)

# Load a regional layer so that you can summarize by region

################################################################################
# Perform final overlay
################################################################################

data_base <- 'O:/Data'
get_country_poly <- function() {
    # TODO: Code to pull proper country based on iso3, hardcoded now for speed
    readOGR(file.path(data_base, 'Global', 'GADM'), 'TZA_adm0')
}
aoi <- get_country_poly()

norm_layer <- function(x) {
    x <- (x - cellStats(x, 'min'))
    x <- x / cellStats(x, 'max')
}

save_suitability <- function(x, name) {
    names(x) <- NULL
    writeRaster(x, filename=paste0(name, '.tif'), overwrite=TRUE)
    v <- rasterToPolygons(x)
    writeOGR(v, paste0(name, '.kml'), 'layer', driver='KML', overwrite=TRUE)
}

# TODO: Add in current potential yields

# Yield gap as difference between potential and actual production
yield_gap <- raster('AGRA_TZA_ygap_diff.tif')
yield_gap_norm <- norm_layer(yield_gap)
yield_diff_cc <- raster('AGRA_TZA_acy_diff.tif')

pas <- raster('AGRA_TZA_wdpa.tif')

fc30 <- raster('AGRA_forest_2015_30.tif')
fc50 <- raster('AGRA_forest_2015_50.tif')

pop_density <- norm_layer(raster('AGRA_TZA_pop_density.tif'))
road_density <- norm_layer(raster('AGRA_TZA_groads_density.tif'))

stunting <- norm_layer(raster('AGRA_TZA_DHS_stunted.tif'))
underweight <- norm_layer(raster('AGRA_TZA_DHS_underweight.tif'))
wasted <- norm_layer(raster('AGRA_TZA_DHS_wasted.tif'))

plot(stack(yield_gap_norm, yield_diff_cc))

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
    stopifnot(model %in% c(0:3))
    if (model == 0) {
        s <- yield_gap_norm
        name <- paste0('AGRA_TZA_suitability_model_', model)
    } else if (model == 1) {
        s <- yield_gap_norm * (yield_diff_cc > -5)
        name <- paste0('AGRA_TZA_suitability_model_', model)
    } else if (model == 2) {
        s <- yield_gap_norm * (yield_diff_cc > -5) * (1 - fc50) * (1 - pas)
        name <- paste0('AGRA_TZA_suitability_model_', model)
    } else if (model == 3) {
        s <- yield_gap_norm * (yield_diff_cc > -5) * (1 - fc50) * (1 - pas) *
               norm_layer(road_w * road_density + pop_w * pop_density + health_w * calc_health())
        name <- paste0('AGRA_TZA_suitability_model_', model, '_p', pop_w, '_r', road_w, '_h', health_w)
    }
    s <- norm_layer(s)
    names(s) <- NULL
    save_suitability(s, name)
    return(s)
}

m0 <- calc_suitability(0)
m1 <- calc_suitability(1)
m2 <- calc_suitability(2)
m3_w1r1h1 <- calc_suitability(3, pop_w=1, road_w=1, health_w=1)
m3_w1r1h0 <- calc_suitability(3, pop_w=1, road_w=1, health_w=0)
m3_w0r0h1 <- calc_suitability(3, pop_w=0, road_w=0, health_w=1)

plot_quantile <- function(m, n) {
    df <- as.data.frame(as(m > quantile(m, n), "SpatialPixelsDataFrame"))
    colnames(df) <- c("value", "x", "y")
    df <- df[df$value != 0, ]
    r <- ggplot() + geom_tile(data=df, aes(x=x, y=y, fill=value), alpha=0.8) + 
        coord_equal() +
        geom_polygon(data=aoi, aes(x=long, y=lat, group=group), fill=NA, color="grey50", size=0.25) +
        theme_map() +
        guides(fill=FALSE)
    return(r)
}

calc_prod_increase_by_quantile <- function(m, n) {
    r <- m > quantile(m, n)
    sum(getValues(yield_gap * r), na.rm=TRUE)
}

plot_results <- function(name, m) {
    a <- plot_quantile(m, .95)
    b <- plot_quantile(m, .90)
    c <- plot_quantile(m, .75)
    d <- foreach(q=c(.95, .9, .75), .combine=rbind) %do% {
        return(data.frame(q=q, p=calc_prod_increase_by_quantile(m, q)))
    }
    d$q <- ordered(d$q, levels=d$q)
    main <- ggplot(d) + geom_bar(aes(q, p), stat='identity') +
        xlab('Quantile') +
        ylab('Production increase (1000s of tons)') +
        theme_bw(base_size=8)
    grid.arrange(grobs=list(main, a, b, c), layout_matrix=rbind(c(1,1,1), c(2, 3, 4)))
    ggsave(filename=paste0(name, '.png'), width=6, height=4, dpi=300)
}

plot_results('Model 0', m0)
