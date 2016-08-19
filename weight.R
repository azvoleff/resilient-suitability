library(raster)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggthemes)
library(rgdal)
library(rgeos)
library(grid)
library(gridExtra)
library(foreach)
library(gfcanalysis)

###############################################################################
# Make plots

data_base <- 'O:/Data'
get_country_poly <- function() {
    # TODO: Code to pull proper country based on iso3, hardcoded now for speed
    readOGR(file.path(data_base, 'Global', 'GADM'), 'TZA_adm0')
}
aoi <- get_country_poly()

regions <- readOGR(file.path(data_base, 'Global', 'GADM'), 'TZA_adm1')
# Calculate area of each region in hectares, after converting to UTM 37S
region_area <- gArea(spTransform(regions, CRS('+init=epsg:32737')), byid=TRUE) / (100*100)
agra_regions <- c('Rukwa',
                  'Manyara',
                  'Ruvuma',
                  'Kilimanjaro',
                  'Arusha',
                  'Kigoma',
                  'Iringa',
                  'Morogoro',
                  'Kagera',
                  'Mbeya')
regions$agra_region <- FALSE
regions$agra_region[regions$NAME_1 %in% agra_regions] <- TRUE

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

# Yield gap as difference between potential and actual production. Note it is 
# in 1000s of tons, so convert it to tons
yg <- raster('AGRA_TZA_ygap_diff.tif') * 1000
yg_norm <- norm_layer(yg)
yield_diff_cc <- raster('AGRA_TZA_acy_diff.tif')
yield_cur <- raster('AGRA_TZA_acy_cur.tif')
yield_fut <- raster('AGRA_TZA_acy_fut.tif')

# Note that calc_pixel_areas returns areas for each line of the raster, so need 
# to rep by ncols
pixel_areas_ha <- gfcanalysis:::calc_pixel_areas(yg) / (100 * 100)
pixel_areas_ha <- raster(matrix(rep(rev(pixel_areas_ha), each=ncol(yg)), 
                                nrow=nrow(yg), ncol=ncol(yg), byrow=TRUE), template=yg)

yg_per_ha <- yg / pixel_areas_ha

pas <- raster('AGRA_TZA_wdpa.tif')

fc30 <- raster('AGRA_forest_2015_30.tif')
fc50 <- raster('AGRA_forest_2015_50.tif')

pop_density <- norm_layer(raster('AGRA_TZA_pop_density.tif'))
road_density <- norm_layer(raster('AGRA_TZA_groads_density.tif'))

stunting <- norm_layer(raster('AGRA_TZA_DHS_stunted.tif'))
underweight <- norm_layer(raster('AGRA_TZA_DHS_underweight.tif'))
wasted <- norm_layer(raster('AGRA_TZA_DHS_wasted.tif'))

# Calculate difference in agro-climatic yield with climate change, and mask out 
# areas that will see a decline in agro-climatic yield

calc_health <- function(stunting_w=1, underweight_w=1, wasted_w=1) {
    return(norm_layer(stunting_w * stunting + underweight_w * underweight + 
                      wasted_w * wasted))
}

# TODO: potential for irrigation, market stresses and shocks
# TODO: mask out urban areas

calc_suitability <- function(model, pop_w, road_w, health_w) {
    stopifnot(model %in% c(0:3))
    if (model == 0) {
        s <- yg_norm
        name <- paste0('AGRA_TZA_suitability_model_', model)
    } else if (model == 1) {
        s <- yg_norm * (yield_diff_cc > -5)
        name <- paste0('AGRA_TZA_suitability_model_', model)
    } else if (model == 2) {
        s <- yg_norm * (yield_diff_cc > -5) * (1 - fc50) * (1 - pas)
        name <- paste0('AGRA_TZA_suitability_model_', model)
    } else if (model == 3) {
        s <- yg_norm * (yield_diff_cc > -5) * (1 - fc50) * (1 - pas) *
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
m3_p1r1h1 <- calc_suitability(3, pop_w=1, road_w=1, health_w=1)
m3_p1r1h0 <- calc_suitability(3, pop_w=1, road_w=1, health_w=0)
m3_p0r0h1 <- calc_suitability(3, pop_w=0, road_w=0, health_w=1)

###############################################################################
# Make plots

# Note that SAGCOT wants to transform 350,000 ha

# Make a map based on a target total potential production. So intensify areas 
# in order of maximum suitability until a given total production increase 
# target is reached
#
# m is a suitability model, y is potential yield, n is the target total 
# production
cum_prod_cutoff <- function(m, y, n) {
    s <- data.frame(v=getValues(m),
                    gap=getValues(yg))
    s <- arrange(s, -v)
    s$gap_cumsum <- cumsum(s$gap)
    s$v[which(s$gap_cumsum >= n)[1]]
}

# Make a map based on a target total area intensified. So intensify areas in 
# order of maximum suitability until a given target area is reached
#
# m is a suitability model, y is potential yield, n is the target area
#
cum_ha_cutoff <- function(m, y, n) {
    s <- data.frame(v=getValues(m),
                    gap=getValues(yg),
                    area=getValues(pixel_areas_ha))
    s <- arrange(s, -v)
    s$area_cumsum <- cumsum(s$area)
    s$v[which(s$area_cumsum >= n)[1]]
}

# Plot areas where suitability weight is greater than or equal to a certain 
# value
plot_areas <- function(m, n) {
    df <- as.data.frame(as(m >= n, "SpatialPixelsDataFrame"))
    colnames(df) <- c("value", "x", "y")
    df <- df[df$value != 0, ]
    r <- ggplot() + geom_tile(data=df, aes(x=x, y=y, fill=value), alpha=0.8) + 
        coord_equal() +
        geom_polygon(data=regions, aes(x=long, y=lat, group=group), fill=NA, color="grey50", size=0.15) +
        theme_map() +
        guides(fill=FALSE)
    return(r)
}

plot_results <- function(name, m) {
    ###########################################################################
    # Quantile based plots
    d_quantile <- foreach(q=c(.95, .9, .75), .combine=rbind) %do% {
        threshold <- quantile(m, q)
        p <- cellStats(yg * (m > threshold), 'sum')
        return(data.frame(q=q, p=p, threshold=threshold))
    }
    d_quantile$label <- paste0((1 - d_quantile$q)*100, '%')
    d_quantile$label <- ordered(d_quantile$label, levels=c('5%', '10%', '25%'))
    # Make maps showing the areas chosen for intensification
    quantile_maps <- lapply(d_quantile$threshold, function(n) plot_areas(m, n))

    main <- ggplot(d_quantile) + geom_bar(aes(label, p/1000), stat='identity') +
        xlab('Percentage of area intensified') +
        ylab('Potential production\nincrease (1000s of tons)') +
        ggtitle(name) +
        theme_bw(base_size=8) +
        theme(panel.grid.major.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.ticks.x=element_blank())
    p <- grid.arrange(grobs=c(list(main), quantile_maps), layout_matrix=rbind(c(1,1,1), c(2, 3, 4)))
    ggsave(p, filename=paste0(name, '_quantiles.png'), width=6, height=4, dpi=300)

    ###########################################################################
    # Area-based plots - remember SAGCOT target is 350,000 ha
    d_area <- foreach(n=c(3.5e5, 1e6, 5e6), .combine=rbind) %do% {
        threshold <- cum_ha_cutoff(m, yg, n)
        return(data.frame(area=n,
                          p=cellStats(yg * (m >= threshold), 'sum'), 
                          threshold=threshold))
    }
    d_area$label <- paste0(prettyNum(d_area$area, big.mark=',', scientific=FALSE), ' ha')
    d_area$label <- ordered(d_area$label, levels=d_area$label[order(d_area$area)])
    # Make maps showing the areas chosen for intensification
    area_maps <- lapply(d_area$threshold, function(n) plot_areas(m, n))

    main <- ggplot(d_area) + geom_bar(aes(label, p/1000), stat='identity') +
        xlab('Area intensified') +
        ylab('Total increase in production\n(1000s of tons)') +
        ggtitle(name) +
        theme_bw(base_size=8) +
        theme(panel.grid.major.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.ticks.x=element_blank())
    p <- grid.arrange(grobs=c(list(main), area_maps), layout_matrix=rbind(c(1,1,1), c(2, 3, 4)))
    ggsave(p, filename=paste0(name, '_byarea.png'), width=6, height=4, dpi=300)

    # # Average total potential prod increase in tons / ha. Note that yield gap 
    # # is in 1000s of tons, so need to convert to tons
    # mean((yg*1000/pixel_areas_ha)[m >= r])

    ###########################################################################
    # Regional plots, based on expansion to a land area equal to the largest 
    # area set above for area-based plots.
    yg_masked <- mask(yg, m > d_area$threshold[nrow(d_area)], maskvalue=FALSE)
    yg_per_ha_masked <- mask(yg_per_ha, m > d_area$threshold[nrow(d_area)], maskvalue=FALSE)
    prod_byreg <- data.frame(Name=regions$NAME_1,
                             # Make output units be 1000s of tons
                             prod_k_tons=raster::extract(yg_masked/1000, regions, fun=sum, na.rm=TRUE),
                             # Make output units be tons / ha
                             prod_ton_ha=raster::extract(yg_per_ha_masked, regions, fun=mean, na.rm=TRUE))

    p <- ggplot(prod_byreg) +
        geom_bar(aes(factor(Name, levels=Name[order(prod_k_tons, decreasing=TRUE)]), prod_k_tons), stat='identity') +
        ylab('Total increase in production\n(1000s of tons)') +
        theme_bw(base_size=8) +
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
              panel.grid.major.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank())
    ggsave(p, filename=paste0(name, '_byregion_tons.png'), width=6, height=4, dpi=300)

    p <- ggplot(prod_byreg) +
        geom_bar(aes(factor(Name, levels=Name[order(prod_ton_ha, decreasing=TRUE)]), prod_ton_ha), stat='identity') +
        ylab('Average potential increase in production\n(tons / hectare)') +
        theme_bw(base_size=8) +
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
              panel.grid.major.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank())
    ggsave(p, filename=paste0(name, '_byregion_tonsperha.png'), width=6, height=4, dpi=300)

}

plot_results('Model 0', m0)
plot_results('Model 1', m1)
plot_results('Model 2', m2)
plot_results('Model 3 (p1r1h1)', m3_p1r1h1)
plot_results('Model 3 (p0r0h1)', m3_p0r0h1)
plot_results('Model 3 (p1r1h0)', m3_p1r1h0)
