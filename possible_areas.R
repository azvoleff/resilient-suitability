# Preserve lands that provide key ecosystem services
#
# Exclude areas from consideration based on WDPA, Forest Cover (Hansen et al.  
# 2013) and freshwater availability from WaterWorld.
get_forest_areas <- function(iso3='TZA') {
    output_folder <- 'H:/Data/GFC_Product'
    aoi <- get_country_poly()
    aoi <- gUnaryUnion(aoi)
    tiles <- calc_gfc_tiles(get_country_poly())
    download_tiles(tiles, output_folder)
    gfc_extract <- extract_gfc(aoi, output_folder)
}

excluded_areas <- function(iso3='TZA') {
    forests <- get_forest_areas()
    
    ##########################################################################
    ### Protected areas
    dataset <- "H:/Data/WDPA/WDPA_June2016/WDPA_June2016-shapefile-polygons.shp"
    wdpa <- setup_vector_layer(dataset)
    wdpa <- gUnaryUnion(wdpa)
    wdpa_ok <- rasterize(wdpa, base, 0, background=1)

    wdpa_ok_poly <- rasterToPolygons(wdpa_ok)
    writeOGR(wdpa_ok_poly, 'AGRA_TZA_wdpa_ok.kml', 'wdpa_ok', driver='KML', 
             overwrite=TRUE)

    ##########################################################################
    ### Water
}

poss <- poss * wdpa_ok

