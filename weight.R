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
yg <- raster('AGRA_TZA_ygap_r_diff.tif') * 1000
yg_i <- raster('AGRA_TZA_ygap_i_diff.tif') * 1000
yg_ir <- raster('AGRA_TZA_ygap_ir_diff.tif') * 1000
# Top code yield gap at 6000 tons / ha
#yg[yg > 6000] <- 6000
yg_norm <- norm_layer(yg)
yield_diff_cc <- raster('AGRA_TZA_acy_diff.tif')

# Load current and future agro-climatic yield
acy_cur <- raster('AGRA_TZA_acy_cur.tif')
acy_fut <- raster('AGRA_TZA_acy_fut.tif')

# Load current rainfed yield (tons/ha)
y_cur_r <- raster('AGRA_TZA_cur_r.tif')
# Load current irrigated yield (tons/ha)
y_cur_r <- raster('AGRA_TZA_cur_r.tif')
# Load current agro-climatic yield for high input rainfed maize (in kg / ha), 
# convert to tons/ha
acy_r_h <- raster('AGRA_TZA_acy_r_h.tif') / 1000
# Load current agro-climatic yield for low input rainfed maize (in kg / ha), 
# convert to tons/ha
acy_r_l <- raster('AGRA_TZA_acy_r_l.tif') / 1000

yg_r_l <- acy_r_l - y_cur_r
yg_r_h <- acy_r_h - y_cur_r

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

# Make mask for AGRA regions
agra_region_mask <- rasterize(regions, wasted, "agra_region")

# Calculate difference in agro-climatic yield with climate change, and mask out 
# areas that will see a decline in agro-climatic yield

calc_health <- function(stunting_w=1, underweight_w=1, wasted_w=1) {
    return(norm_layer(stunting_w * stunting + underweight_w * underweight + 
                      wasted_w * wasted))
}

# TODO: potential for irrigation, market stresses and shocks
# TODO: mask out urban areas

calc_suitability <- function(model, pop_w, road_w, health_w) {
    stopifnot(model %in% c(0:4))
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
    } else if (model == 4) {
        s <- yg_norm * (yield_diff_cc > -5) * (1 - fc50) * (1 - pas) * agra_region_mask *
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
m4_p1r1h1 <- calc_suitability(4, pop_w=1, road_w=1, health_w=1)

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

names <- c('Model 0', 'Model 1', 'Model 2', 'Model 3 (p1r1h1)',
           'Model 3 (p0r0h1)', 'Model 3 (p1r1h0)', 'Model 4 (p1r1h1)')
models <- c(m0, m1, m2, m3_p1r1h1, m3_p0r0h1, m3_p1r1h0, m4_p1r1h1)

###############################################################################
### Make plots of intensification areas
p_by_area <- foreach(m=models, model=names, .combine=rbind) %do% {
    d <- foreach(n=c(3.5e5, 1e6, 5e6), .combine=rbind) %do% {
        threshold <- cum_ha_cutoff(m, yg, n)
        # area here is the area intensified
        return(data.frame(area=n,
                          p=cellStats(yg * (m >= threshold), 'sum'), 
                          threshold=threshold))
    }
    d$label <- paste0(prettyNum(d$area, big.mark=',', scientific=FALSE), ' ha')
    d$label <- ordered(d$label, levels=d$label[order(d$area)])
    d$model <- model
    d
}

plot_area_intensified <- function(d, name, m) {
    d <- filter(d, model == name)
    # Make maps showing the areas chosen for intensification
    area_maps <- lapply(d$threshold, function(n) plot_areas(m, n))
    main <- ggplot(d) + geom_bar(aes(label, p/1000), stat='identity') +
        xlab('Area intensified') +
        ylab('Total increase in production\n(1000s of tons)') +
        #ggtitle(name) +
        theme_bw(base_size=8) +
        theme(panel.grid.major.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.ticks.x=element_blank())
    p <- grid.arrange(grobs=c(list(main), area_maps), layout_matrix=rbind(c(1,1,1), c(2, 3, 4)))
    ggsave(p, filename=paste0(name, '_byarea.png'), width=6, height=4, dpi=300)
}

# Now plot each model
foreach(m=models, name=names, .combine=rbind) %do% {
    plot_area_intensified(p_by_area, name, m)
}

###############################################################################
### Make plots of intensified yields by region based on expansion to a land 
### area equal to the largest area set above for the intensification area
### plots.
mask_yield <- function(y, m, threshold) {
    y[m < threshold] <- NA
    y[is.na(m)] <- NA
    return(y)
}

p_by_region <- foreach(m=models, name=names, .combine=rbind) %do% {
    threshold <- filter(p_by_area, model == name, area == max(area))$threshold
    yg_masked <- mask_yield(yg, m, threshold)
    yg_per_ha_masked <- mask_yield(yg_per_ha, m, threshold)
    data.frame(model=name,
               region=regions$NAME_1,
               # Make output units be 1000s of tons
               prod_k_tons=raster::extract(yg_masked/1000, regions, fun=sum, na.rm=TRUE),
               # Make output units be tons / ha
               prod_ton_ha=raster::extract(yg_per_ha_masked, regions, fun=mean, na.rm=TRUE))
}

plot_regional_stats <- function(d, name, m) {
    p <- ggplot(d) +
        geom_bar(aes(factor(region, levels=region[order(prod_k_tons, decreasing=TRUE)]), prod_k_tons), stat='identity') +
        ylab('Total increase in production\n(1000s of tons)') +
        theme_bw(base_size=8) +
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
              panel.grid.major.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank())
    ggsave(p, filename=paste0(name, '_byregion_tons.png'), width=6, height=4, dpi=300)

    p <- ggplot(d) +
        geom_bar(aes(factor(region, levels=region[order(prod_ton_ha, decreasing=TRUE)]), prod_ton_ha), stat='identity') +
        ylab('Average potential increase in production\n(tons / hectare)') +
        theme_bw(base_size=8) +
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
              panel.grid.major.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank())
    ggsave(p, filename=paste0(name, '_byregion_tonsperha.png'), width=6, height=4, dpi=300)

}

# Now plot each model
foreach(m=models, name=names, .combine=rbind) %do% {
    d <- filter(p_by_region, model == name)
    plot_regional_stats(d, name, m)
}

###############################################################################
### Additional presentation plots

p_by_region_agra <- filter(p_by_region,
                           region %in% agra_regions,
                           model %in% c('Model 0', 'Model 4 (p1r1h1)'))
p_by_region_agra$model <- ordered(p_by_region_agra$model,
                            levels=c('Model 0', 'Model 4 (p1r1h1)'),
                            labels=c('Standard approach', 'Resilient suitability'))

ggplot(p_by_region_agra) +
    geom_bar(aes(factor(region, levels=region[order(prod_ton_ha, decreasing=TRUE)]), prod_ton_ha), stat='identity') +
    facet_grid(model~.) +
    ylab('Average potential increase in production\n(tons / hectare)') +
    theme_bw(base_size=8) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank())
ggsave(filename=paste0('byregion_tonsperha_AGRA_regions.png'), width=6, height=4, dpi=300)

ggplot(p_by_region_agra) +
    geom_bar(aes(factor(region, levels=region[order(prod_k_tons, decreasing=TRUE)]), prod_k_tons), stat='identity') +
    facet_grid(model~.) +
    ylab('Total increase in production\n(1000s of tons)') +
    theme_bw(base_size=8) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank())
ggsave(filename=paste0('byregion_prod_k_tons_AGRA_regions.png'), width=6, height=4, dpi=300)
