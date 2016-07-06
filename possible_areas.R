# Identify possible areas for intensification
#
# Considers projected changes in agro-climatic suitability and current (year 
# 2000) yield gaps.
possible_maize_areas <- function(iso3='TZA') {
    ##########################################################################
    ### Find areas where yields should increase or stay stable with clim chg

    # Load current and future agro-climatic yield
    # TODO: need to use median of all climate model outputs
    
    # This is agroc-climatically attainable yield for intermediate input level 
    # rain-fed maize for future period 2050s from Hadley CM3 B2 scenario
    acy_fut <- raster(file.path(data_base, 'HarvestChoice', 'res02_h3a22020i_maiz150b_yld.tif'))
    acy_fut <- setup_raster_layer(acy_fut)
    acy_cur <- raster(file.path(data_base, 'HarvestChoice', 'res02_crav6190i_maiz150b_yld.tif'))
    acy_cur <- setup_raster_layer(acy_cur)

    acy_diff <- ((acy_fut - acy_cur) / acy_cur) * 100
    acy_diff_poly <- polygonize(acy_diff)
    # Save difference in agro-climatic yield with climate change for use in 
    # plots.
    writeOGR(acy_diff_poly, 'AGRA_TZA_acy_diff.kml', 'acy_diff', driver='KML', 
             overwrite=TRUE)

    # Calculate difference in agro-climatic yield with climate change, and mask 
    # out areas that will see a decline in agro-climatic yield
    acy_exclude <- acy_diff >= -5

    acy_exclude_poly <- polygonize(acy_exclude)
    writeOGR(acy_exclude_poly, 'AGRA_TZA_acy_exclude.kml', 'acy_exclude', 
             driver='KML', overwrite=TRUE)

    ##########################################################################
    ### Find areas where large yield gap exists

    # Ratio of actual and potential yield for rain-fed maize
    ygap_ratio <- raster(file.path(data_base, 'HarvestChoice', 
                                   'gap2000_r_mze_2000_yga_cl.tif'))
    ygap_ratio <- setup_raster_layer(ygap_ratio)

    # Difference of potential and actual production for rain-fed maize
    ygap_diff <- raster(file.path(data_base, 'HarvestChoice', 
                                  'gap2000_r_mze_2000_qga.tif'))
    ygap_diff <- setup_raster_layer(ygap_diff)

    ygap_diff_poly <- polygonize(ygap_diff)
    writeOGR(ygap_diff_poly, 'AGRA_TZA_ygap_diff.kml', 'ygap_diff', 
             driver='KML', overwrite=TRUE)

    # Normalize by largest gap
    ygap_diff_norm <- ygap_diff / cellStats(ygap_diff, 'max')

    ##########################################################################
    ### TODO: Exclude degraded areas


    return(ygap_diff * acy_exclude)
}

poss <- possible_maize_areas()
poss_poly <- polygonize(poss)
writeOGR(poss_poly, 'AGRA_TZA_poss_areas.kml', 'AGRA_TZA_poss_areas', 
         driver='KML', overwrite=TRUE)
