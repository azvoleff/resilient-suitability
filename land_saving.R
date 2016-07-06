# Preserve lands that provide key ecosystem services
#
# Exclude areas from consideration based on WDPA, Forest Cover (Hansen et al.  
# 2013) and freshwater availability from WaterWorld.
get_forest_areas <- function(iso3='TZA') {
    out_file <- file.path(data_base, 'AGRA/gfc_extract.tif')
    output_folder <- file.path(data_base, 'GFC_Product')
    if (!file_test('-f', out_file)) {
        aoi <- get_country_poly()
        aoi <- gUnaryUnion(aoi)
        tiles <- calc_gfc_tiles(get_country_poly())
        download_tiles(tiles, output_folder)
        gfc_extract <- extract_gfc(aoi, output_folder, filename=out_file)
    } else {
        gfc_extract <- stack(out_file)
    }
    threshold <- 50 # threshold to be considered forest, in percent
    # DEBUG ONLY
    #gfc_extract <- crop(gfc_extract, readOGR(file.path(data_base, 'AGRA'), 'test_area'))
    # /DEBUG ONLY
    
    forest_2015_10km <- aggregate(subset(gfc_extract, c(1, 4)),
        fact=c(round(res(base)/res(gfc_extract)), 2),
        expand=FALSE,
        function(x, na.rm) {
            b1 <- x[1:(length(x)/2)]
            b2 <- x[(length(x)/2+1):length(x)]
            f2000 <- b1 > threshold
            f2015 <- f2000 & (b2 == 0)
            f2015 / f2015
            ux <- unique(f2015)
            ux[which.max(tabulate(match(f2015, ux)))]
        })
    forest_2015_10km_poly <- polygonize(forest_2015_10km)
    writeOGR(forest_2015_10km_poly, paste0('AGRA_forest_2015_10km_', threshold, 'pct.kml'),
             'forest', driver='KML', overwrite=TRUE)

    return(forest_2015_10km)
}

excluded_areas <- function(iso3='TZA') {
    forests <- get_forest_areas()
    
    ##########################################################################
    ### Protected areas
    dataset <- file.path(data_base, "WDPA/WDPA_June2016/WDPA_June2016-shapefile-polygons.shp")
    wdpa <- setup_vector_layer(dataset)
    wdpa <- gUnaryUnion(wdpa)
    wdpa_ok <- rasterize(wdpa, base, 0, background=1)

    wdpa_ok_poly <- polygonize(wdpa_ok)
    writeOGR(wdpa_ok_poly, 'AGRA_TZA_wdpa_ok.kml', 'wdpa_ok', driver='KML', 
             overwrite=TRUE)

    ##########################################################################
    ### Forests

    ##########################################################################
    ### Water
}

poss <- poss * excluded_areas()
