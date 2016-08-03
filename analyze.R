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
iso3 <- 'TZA'
res_degrees <- .08333333
clipsrc <- file.path(data_base, 'Global', 'GADM', 'TZA_adm0.shp')

# Function of population, road access
get_country_poly <- function() {
    # TODO: Code to pull proper country based on iso3, hardcoded now for speed
    readOGR(file.path(data_base, 'Global', 'GADM'), 'TZA_adm0')
}

setup_base_layer <- function() {
    # TODO: Automatically generate base layer as a 10x10km grid (in WGS84, so 
    # actually in degrees...) covering the target country
    r <- raster(file.path(data_base, 'HarvestChoice', 'res02_csa22020i_maiz150b_yld.tif'))
    crop(r, get_country_poly())
}
base <- setup_base_layer()

setup_raster_layer <- function(x, method='ngb') {
    if (!compareCRS(x, base)) {
        x <- projectRaster(x, crs=proj4string(base), method=method)
    }
    if ((xres(base)) > xres(x) | (yres(x) > yres(base))) {
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

polygonize <- function(x) {
    x <- mask(x, get_country_poly())
    rasterToPolygons(x)
}

save_raster <- function(x, name) {
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
    ygap_ratio <- raster(file.path(data_base, 'HarvestChoice', 
                                   'gap2000_r_mze_2000_yga_cl.tif'))
    ygap_ratio <- setup_raster_layer(ygap_ratio)

    # Difference of potential and actual production for rain-fed maize
    ygap_diff <- raster(file.path(data_base, 'HarvestChoice', 
                                  'gap2000_r_mze_2000_qga.tif'))
    ygap_diff <- setup_raster_layer(ygap_diff)

    save_raster(ygap_diff, 'AGRA_TZA_ygap_diff')

    # Normalize by largest gap
    #ygap_diff_norm <- ygap_diff / cellStats(ygap_diff, 'max')
    return(ygap_diff)
}

##########################################################################
# Account for future changes in agro-climatic potential yield
# TODO: need to use median of all climate model outputs
calc_acy_cc_diff_pct <- function() {
    # This is agroc-climatically attainable yield for intermediate input level 
    # rain-fed maize for future period 2050s from Hadley CM3 B2 scenario
    acy_fut <- raster(file.path(data_base, 'HarvestChoice', 'res02_h3a22020i_maiz150b_yld.tif'))
    acy_fut <- setup_raster_layer(acy_fut)
    acy_cur <- raster(file.path(data_base, 'HarvestChoice', 'res02_crav6190i_maiz150b_yld.tif'))
    acy_cur <- setup_raster_layer(acy_cur)

    # Calculate difference in agro-climatic yield with climate change as a 
    # percentage of current agroclimatic yield
    acy_diff <- ((acy_fut - acy_cur) / acy_cur) * 100
    save_raster(acy_diff, 'AGRA_TZA_acy_diff')

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
    # DEBUG ONLY
    #gfc_extract <- crop(gfc_extract, readOGR(file.path(data_base, 'AGRA'), 'test_area'))
    # /DEBUG ONLY
    
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
    save_raster(forest_2015, paste0('AGRA_forest_2015_', threshold))
    return(forest_2015)
}

# fc_30 <- calc_forest_areas(30)
# fc_50 <- calc_forest_areas(50)

# Exclude protected areas from consideration
calc_prot_areas <- function() {
    dataset <- file.path(data_base, "WDPA/WDPA_June2016/WDPA_June2016-shapefile-polygons.shp")
    wdpa <- setup_vector_layer(dataset)
    wdpa <- gUnaryUnion(wdpa)
    wdpa_ok <- rasterize(wdpa, base, 0, background=1)
    save_raster(wdpa_ok, 'AGRA_TZA_wdpa_ok')
    return(wdpa_ok)
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
    writeOGR(roads, 'AGRA_TZA_groads.kml', 'groads', driver='KML', 
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
    save_raster(road_density, 'AGRA_TZA_groads_density')
    return(road_density)
}

calc_pop_density <- function() {
    # Compute population density per grid square
    pop <- raster(file.path(data_base, 'Landscan', 'Landscan2014', 'landscan_2014.tif'))
    pop <- crop(pop, get_country_poly())
    writeRaster(pop, 'AGRA_TZA_pop_density_original.tif', overwrite=TRUE)

    pop <- aggregate(pop, fact=c(round(res(base)/res(pop))), expand=FALSE, 
                     fun=sum)
    # Convert to pop/km^2
    pop <- pop/(10*10)
    save_raster(pop, 'AGRA_TZA_pop_density')
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

    dhs_vars <- get_indic(vars, countryIds='TZ')

    dhs_vars$SurveyYear <- as.numeric(dhs_vars$SurveyYear)
    dhs_vars$Value <- as.numeric(dhs_vars$Value)

    names(dhs_vars)[names(dhs_vars) == "RegionId"] <- 'REG_ID'

    # Filter to only include most recent data
    dhs_vars <- dhs_vars[dhs_vars$SurveyYear == max(dhs_vars$SurveyYear), ]

    #ggplot(dhs_vars) + geom_point(aes(Indicator, Value))

    # Join Tanzania polygons
    dhs_regions_database <- file.path(data_base, 'DHS', 'DHS_Regions', 'DHS_Regions_SDR.gdb')
    out_file <- tempfile(fileext='.shp')
    ogr2ogr(dhs_regions_database, out_file, clipsrcsql="select * from DHS_Regions_SDR where ISO='TZ'")
    dhs_regions <- readOGR(dirname(out_file), file_path_sans_ext(basename(out_file)))
    dhs_regions <- dhs_regions[dhs_regions$ISO == 'TZ', ]

    dhs_regions <- dhs_regions[dhs_regions$REG_ID %in% dhs_vars$REG_ID, ]

    dhs_vars <- spread(select(dhs_vars, Indicator, Value, REG_ID), Indicator, Value)

    dhs_regions@data <- left_join(dhs_regions@data, dhs_vars)

    writeOGR(dhs_regions, 'AGRA_TZA_DHS_indicators.geojson', 
             'AGRA_TZA_DHS_indicators', driver='GeoJSON')

    # Make rasters of each dhs layer
    stunted <- rasterize(dhs_regions, base, "Children stunted", background=NA)
    save_raster(stunted, 'AGRA_TZA_DHS_stunted')
    wasted <- rasterize(dhs_regions, base, "Children wasted", background=NA)
    save_raster(wasted, 'AGRA_TZA_DHS_wasted')
    underweight <- rasterize(dhs_regions, base, "Children underweight", background=NA)
    save_raster(underweight, 'AGRA_TZA_DHS_underweight')

    return(stack(stunted, wasted, underweight))
}

################################################################################
# Weight by exposure to market price shocks
################################################################################
