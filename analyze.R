library(httr)
library(rgeos)
library(maptools)
library(raster)
library(gdalUtils)
library(rgdal)
library(tools)
library(dplyr)
library(RJSONIO)
library(tidyr)
library(gfcanalysis)

#data_base <- 'H:/Data'
data_base <- 'O:/Data'
iso3 <- 'EA'
res_degrees <- .08333333
clipsrc <- file.path(data_base, 'Global', 'GADM', 'TZA_adm0.shp')

# Function of population, road access
get_country_poly <- function() {
    readOGR(file.path(data_base, 'Global'), 'RWA_TZA_UGA')
}

setup_base_layer <- function() {
    # TODO: Automatically generate base layer as a 10x10km grid (in WGS84, so 
    # actually in degrees...) covering the target country
    r <- raster(file.path(data_base, 'GAEZ', 'res02_csa22020i_maiz150b_yld.tif'))
    crop(r, get_country_poly())
}
base <- setup_base_layer()

setup_raster_layer <- function(x, method='ngb') {
    names(x) <- NULL
    if (!compareCRS(x, base)) {
        x <- projectRaster(x, crs=proj4string(base), method=method)
    }
    if (!identical(round(res(base)/res(x)), c(1, 1)) &
        ((xres(base) > xres(x)) | (yres(x) > yres(base)))) {
        if (method == 'ngb') {
            # Aggregate x to the same resolution as base using mode if categorical data
            x <- aggregate(x, round(res(base)/res(x)),
                fun=function(x, na.rm) {
                    ux <- unique(x)
                    ux[which.max(tabulate(match(x, ux)))]
                })
        } else {
            x <- aggregate(x, round(res(base)/res(x)), fun=mean)
        }
    }
    projectRaster(x, base, method=method)
}

setup_vector_layer <- function(dataset) {
    spat <- as.numeric(bbox(base))
    out_file <- tempfile(fileext='.shp')
    ogr2ogr(dataset, out_file, spat=spat, spat_srs=proj4string(base),
            simplify=res_degrees/10)
            #clipsrc=clipsrc, simplify=res_degrees/10)
    readOGR(dirname(out_file), file_path_sans_ext(basename(out_file)))
}

save_raster <- function(x, name) {
    names(x) <- NULL
    x <- mask(x, get_country_poly())
    writeRaster(x, filename=paste0(name, '.tif'), overwrite=TRUE)
    v <- rasterToPolygons(x)
    writeOGR(v, paste0(name, '.kml'), 'layer', driver='KML', overwrite=TRUE)
}

################################################################################
# Make initial map, and exclude areas where yield declines are expected
################################################################################

# Identify possible areas for intensification
#
# Considers projected changes in agro-climatic suitability and current (year 
# 2000) yield gaps.
calc_yield_gap <- function() {
    ##########################################################################
    ### Find areas where large yield gap exists

    # Ratio of actual and potential yield for rain-fed maize
    ygap_ratio <- raster(file.path(data_base, 'GAEZ', 
                                   'gap2000_r_mze_2000_yga_cl.tif'))
    ygap_ratio <- setup_raster_layer(ygap_ratio)

    # Difference of potential and actual production for rain-fed and irrigated 
    # maize
    ygap_ir_diff <- raster(file.path(data_base, 'GAEZ', 
                                  'gap2000_t_mze_2000_qga.tif'))
    ygap_ir_diff <- setup_raster_layer(ygap_ir_diff)
    save_raster(ygap_ir_diff, 'AGRA_EA_ygap_ir_diff')

    # Difference of potential and actual production for irrigated maize
    ygap_i_diff <- raster(file.path(data_base, 'GAEZ', 
                                  'gap2000_i_mze_2000_qga.tif'))
    ygap_i_diff <- setup_raster_layer(ygap_i_diff)
    save_raster(ygap_i_diff, 'AGRA_EA_ygap_i_diff')

    # Difference of potential and actual production for rain-fed maize
    ygap_r_diff <- raster(file.path(data_base, 'GAEZ', 
                                  'gap2000_r_mze_2000_qga.tif'))
    ygap_r_diff <- setup_raster_layer(ygap_r_diff)
    save_raster(ygap_r_diff, 'AGRA_EA_ygap_r_diff')

    # Agro-climatically attainable yield for low-input rain-fed maize in kg per 
    # ha
    acy_cur_r_l <- raster(file.path(data_base, 'GAEZ',
                                    'res02_crav6190l_maiz150b_yld.tif'))
    acy_cur_r_l <- setup_raster_layer(acy_cur_r_l)
    save_raster(acy_cur_r_l, 'AGRA_EA_acy_r_l')

    # Agro-climatically attainable yield for high-input rain-fed maize in kg 
    # per ha
    acy_cur_r_h <- raster(file.path(data_base, 'GAEZ',
                                    'res02_crav6190h_maiz150b_yld.tif'))
    acy_cur_r_h <- setup_raster_layer(acy_cur_r_h)
    save_raster(acy_cur_r_h, 'AGRA_EA_acy_r_h')

    # Yield in 2000 of rainfed maize - tons/ha
    y_cur_r <- raster(file.path(data_base, 'GAEZ',
                                    'act2000_r_mze_2000_yld.tif'))
    y_cur_r <- setup_raster_layer(y_cur_r)
    save_raster(y_cur_r, 'AGRA_EA_cur_r')

    # Yield in 2000 of irrigated maize - tons/ha
    y_cur_i <- raster(file.path(data_base, 'GAEZ',
                                    'act2000_i_mze_2000_yld.tif'))
    y_cur_i <- setup_raster_layer(y_cur_i)
    save_raster(y_cur_i, 'AGRA_EA_cur_i')

    # Yield gap 2000 of irrigated maize - tons/ha
    y_cur_i <- raster(file.path(data_base, 'GAEZ',
                                    'act2000_i_mze_2000_yld.tif'))
    y_cur_i <- setup_raster_layer(y_cur_i)
    save_raster(y_cur_i, 'AGRA_EA_cur_i')

    # Normalize by largest gap
    #ygap_diff_norm <- ygap_diff / cellStats(ygap_diff, 'max')
}

##########################################################################
# Account for future changes in agro-climatic potential yield
# TODO: need to use median of all climate model outputs
calc_acy_cc_diff_pct <- function() {
    # This is agroc-climatically attainable yield for intermediate input level 
    # rain-fed maize for future period 2050s from Hadley CM3 B2 scenario
    acy_fut <- raster(file.path(data_base, 'GAEZ', 'res02_h3a22020i_maiz150b_yld.tif'))
    acy_fut <- setup_raster_layer(acy_fut)
    acy_cur <- raster(file.path(data_base, 'GAEZ', 'res02_crav6190i_maiz150b_yld.tif'))
    acy_cur <- setup_raster_layer(acy_cur)

    save_raster(acy_cur, 'AGRA_EA_acy_cur')
    save_raster(acy_fut, 'AGRA_EA_acy_fut')

    # Calculate difference in agro-climatic yield with climate change as a 
    # percentage of current agroclimatic yield
    acy_diff <- ((acy_fut - acy_cur) / acy_cur) * 100
    save_raster(acy_diff, 'AGRA_EA_acy_diff')

    return(acy_diff)
}

##########################################################################
### TODO: Exclude degraded areas

################################################################################
# Exclude areas supporting natural capital
################################################################################

# Exclude areas from consideration based on Forest Cover (Hansen et al.  2013)
calc_forest_areas <- function(threshold=50) {
    # Forest Cover Hansen et al. (2013)
    out_file <- file.path(data_base, 'AGRA/gfc_extract_EA.tif')
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
    # DEBUG ONLY
    #gfc_extract <- crop(gfc_extract, readOGR(file.path(data_base, 'AGRA'), 'test_area'))
    # /DEBUG ONLY
    
    #TODO: this should be a percent cover rather than a max
    forest_2015 <- aggregate(subset(gfc_extract, c(1, 4)),
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
    forest_2015 <- setup_raster_layer(forest_2015)
    save_raster(forest_2015, paste0('AGRA_EA_forest_2015_', threshold))

    return(forest_2015)
}

# fc_30 <- calc_forest_areas(30)
# fc_50 <- calc_forest_areas(50)

# Exclude protected areas from consideration
calc_prot_areas <- function() {
    dataset <- file.path(data_base, "WDPA/WDPA_June2016/WDPA_June2016-shapefile-polygons.shp")
    wdpa <- setup_vector_layer(dataset)
    wdpa <- gUnaryUnion(wdpa)
    wdpa <- rasterize(wdpa, base, 1, background=0)
    save_raster(wdpa, 'AGRA_EA_wdpa')

    return(wdpa)
}

# TODO: exclude areas important for freshwater availability from WaterWorld

################################################################################
# Weight by population and access
################################################################################

calc_road_density <- function() {

    in_database <- file.path(data_base, 'GROADS', 'gROADS_v1.gdb')
    # TODO: Need to buffer the spat to account for roads that may fall 
    # outside national border but still be closs enough to affect access.
    spat <- as.numeric(bbox(base))
    out_file <- tempfile(fileext='.shp')
    ogr2ogr(in_database, out_file, spat=spat, spat_srs=proj4string(base),
            simplify=res_degrees/10)
            #clipsrc=clipsrc, simplify=res_degrees/10)
    roads <- readOGR(dirname(out_file), file_path_sans_ext(basename(out_file)))

    aoi <- get_country_poly()
    s_srs <- proj4string(aoi)
    aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
    aoi <- gBuffer(aoi, width=5000)
    aoi <- spTransform(aoi, CRS(s_srs))

    roads <- gIntersection(roads, aoi, byid=TRUE)
    roads <- SpatialLinesDataFrame(roads,
                                   data=data.frame(id=1:length(roads), 
                                                   row.names=row.names(roads)))
    writeOGR(roads, 'AGRA_EA_groads.kml', 'groads', driver='KML', 
             overwrite=TRUE)

    # Compute road density per grid square
    roads <- spTransform(roads, CRS("+proj=utm +zone=37 +south +datum=WGS84"))
    road_dens_base <- projectRaster(base, crs=proj4string(roads))

    rs <- raster(road_dens_base)
    rs[] <- 1:ncell(rs)
    rsp <- rasterToPolygons(rs)
    rp <- raster::intersect(roads, rsp)
    rp$length <- gLength(rp, byid=TRUE) / 1000
    x <- tapply(rp$length, rp$layer, sum)
    r <- raster(rs)
    r[as.integer(names(x))] <- x
    # Density is in km/km^2

    road_density <- projectRaster(r, crs=proj4string(base))
    road_density <- setup_raster_layer(road_density)
    save_raster(road_density, 'AGRA_EA_groads_density')

    return(road_density)
}

calc_pop_density <- function() {
    # Compute population density per grid square in 2015
    pop <- raster(file.path(data_base, 'GPWv4', 'gpw-v4-population-count_2015.tif'))
    pop <- crop(pop, get_country_poly())
    pop <- mask(pop, get_country_poly())
    writeRaster(pop, 'AGRA_EA_pop_count_2015_fullres.tif')
    pop <- setup_raster_layer(pop)
    save_raster(pop, 'AGRA_EA_pop_count_2015')

    # Compute population density per grid square in 2020
    pop <- raster(file.path(data_base, 'GPWv4', 'gpw-v4-population-count_2020.tif'))
    pop <- crop(pop, get_country_poly())
    pop <- mask(pop, get_country_poly())
    writeRaster(pop, 'AGRA_EA_pop_count_2020_fullres.tif')
    pop <- setup_raster_layer(pop)
    save_raster(pop, 'AGRA_EA_pop_count_2020')

    return(pop)
}

################################################################################
# Weight by food security related health stress
################################################################################

###############################################################################
### Pick variables

calc_dhs_weights <- function() {
    dhs_api_key <- Sys.getenv('dhs_api_key')
    indicator_list <- fromJSON(paste0('http://api.dhsprogram.com/rest/dhs/indicators?APIkey=',
                                      dhs_api_key, '?returnFields=IndicatorId,Label,Definition'))
    # Unlist the JSON file entries
    indicator_list <- lapply(indicator_list$Data, function(x) {unlist(x)})
    # Convert JSON input to a data frame
    indicator_list <- as.data.frame(do.call("rbind", indicator_list), stringsAsFactors=FALSE)

    grep_list <- function(x) {
        def_rows <- grepl(tolower(x), tolower(indicator_list$Label))
        label_rows <- grepl(tolower(x), tolower(indicator_list$Definition))
        return(indicator_list[(def_rows | label_rows), 1:3])
    }

    get_indic <- function(indicatorIDs, countryIds) {
        # Note DHS uses "NI" for Niger instead of the correct, "NE")
        indicators_string <- paste0("indicatorIds=", paste(indicatorIDs, collapse=","))
        countryIds_string <- paste0("countryIds=", paste(countryIds, collapse=","))
        api_base <- "http://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&APIkey=CVINTL-877527&perpage=5000"
        api_call <- paste(api_base, indicators_string, countryIds_string, "f=json", sep='&')
        # Make repeated calls to retrieve all records as there is a 5000 record 
        # limit per request
        nrecs <- 5000
        i <- 1
        while (nrecs == 5000) {
            this_call <- paste0(api_call, '&page=', i)
            json_file <- fromJSON(this_call)
            # Unlist the JSON file entries
            this_d <- lapply(json_file$Data, function(x) {unlist(x)})
            # Convert JSON input to a data frame
            this_d <- as.data.frame(do.call("rbind", this_d), stringsAsFactors=FALSE)
            if (i == 1) d <- this_d
            else d <- rbind(d, this_d)
            nrecs <- nrow(this_d)
            i <-  i + 1
        }
        d <- tbl_df(d)
    }

    vars <- c('CN_NUTS_C_HA2', # percent children stunted
              'CN_NUTS_C_WA2', # percent children underweight
              'CN_NUTS_C_WH2') # percent children wasted

    dhs_vars <- get_indic(vars, countryIds=c('TZ', 'RW', 'UG'))

    dhs_vars$SurveyYear <- as.numeric(dhs_vars$SurveyYear)
    dhs_vars$Value <- as.numeric(dhs_vars$Value)

    names(dhs_vars)[names(dhs_vars) == "RegionId"] <- 'REG_ID'
    dhs_vars <- spread(select(dhs_vars, Indicator, Value, REG_ID), Indicator, Value)

    #ggplot(dhs_vars) + geom_point(aes(Indicator, Value))

    # Join region polygons
    dhs_regions_database <- file.path(data_base, 'DHS', 'DHS_Regions', 'DHS_Regions_SDR.gdb')
    out_file <- tempfile(fileext='.shp')
    ogr2ogr(dhs_regions_database, out_file, clipsrcsql="select * from DHS_Regions_SDR where ISO in ('TZ', 'UG', 'RW')")
    dhs_regions <- readOGR(dirname(out_file), file_path_sans_ext(basename(out_file)))
    dhs_regions <- dhs_regions[dhs_regions$ISO %in% c('TZ', 'UG', 'RW'), ]

    dhs_regions <- dhs_regions[dhs_regions$REG_ID %in% dhs_vars$REG_ID, ]

    dhs_regions@data <- left_join(dhs_regions@data, dhs_vars)

    # Filter to only include most recent data
    included_regions <- group_by(dhs_regions@data, ISO) %>%
        filter(SVYYEAR == max(SVYYEAR))

    dhs_regions <- dhs_regions[dhs_regions$REG_ID %in% included_regions$REG_ID, ]

    # TZA is representative at two levels. Drop the "Zones"
    dhs_regions <- dhs_regions[dhs_regions$LEVELNA != 'Zones', ]

    writeOGR(dhs_regions, 'AGRA_EA_DHS_indicators.geojson', 
             'AGRA_EA_DHS_indicators', driver='GeoJSON')

    # Make rasters of each dhs layer
    stunted <- rasterize(dhs_regions, base, "Children stunted", background=NA)
    save_raster(stunted, 'AGRA_EA_DHS_stunted')
    wasted <- rasterize(dhs_regions, base, "Children wasted", background=NA)
    save_raster(wasted, 'AGRA_EA_DHS_wasted')
    underweight <- rasterize(dhs_regions, base, "Children underweight", background=NA)
    save_raster(underweight, 'AGRA_EA_DHS_underweight')

    return(stack(stunted, wasted, underweight))
}

################################################################################
# Soils data
################################################################################

process_soils_data <- function() {
    # soil pH x 10 in H20 at depth 0.00m
    ph <- raster(file.path(data_base, 'SoilGrids', 'PHIHOX_M_sl1_250m_ll.tif'))
    ph <- crop(ph, get_country_poly())
    ph <- setup_raster_layer(ph, method='bilinear')
    save_raster(ph, 'AGRA_EA_soils_phihox_m_sl1_10km')

    # cation exchange capacity of soil in cmolc/kg at depth 0.00m
    cation <- raster(file.path(data_base, 'SoilGrids', 'CECSOL_M_sl1_250m_ll.tif'))
    cation <- crop(cation, get_country_poly())
    cation <- setup_raster_layer(cation, method='bilinear')
    save_raster(cation, 'AGRA_EA_soils_cecsol_m_sl1_10km')
    
    # soil organic carbon content (fine earth fraction) in g per kg at depth 
    # 0.00m
    org_carbon <- raster(file.path(data_base, 'SoilGrids', 'ORCDRC_M_sl1_250m_ll.tif'))
    org_carbon <- crop(org_carbon, get_country_poly())
    org_carbon <- setup_raster_layer(org_carbon, method='bilinear')
    save_raster(org_carbon, 'AGRA_EA_soils_orcdrc_m_sl1_10km')

    # total phosphorus
    p_total <- raster(file.path(data_base, 'SoilGrids', 'P.T_M_agg35cm_AF_250m.tif'))
    country_poly <- spTransform(get_country_poly(), proj4string(p_total))
    p_total <- crop(p_total, country_poly)
    p_total <- setup_raster_layer(p_total, method='bilinear')
    save_raster(p_total, 'AGRA_EA_soils_p_t_m_agg35_10km')

    # potassium extractable by Mehlich 3
    k_total <- raster(file.path(data_base, 'SoilGrids', 'K_M_agg35cm_AF_250m.tif'))
    k_total <- crop(k_total, country_poly)
    k_total <- setup_raster_layer(k_total, method='bilinear')
    save_raster(k_total, 'AGRA_EA_soils_k_m_agg35_10km')
    
    # total (organic) nitrogen extractable by wet oxidation
    n_total <- raster(file.path(data_base, 'SoilGrids', 'N_M_agg35cm_AF_250m.tif'))
    n_total <- crop(n_total, country_poly)
    n_total <- setup_raster_layer(n_total, method='bilinear')
    save_raster(n_total, 'AGRA_EA_soils_n_m_agg35_10km')

}

################################################################################
# Weight by exposure to market price shocks
################################################################################
