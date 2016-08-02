###############################################################################
### Nutrition
###############################################################################

# Extract data for selected country code using DHS API

# Save data to KML to map

# Weight results by regional data

# First go:
# potential <- mask(poss, get_country_poly())
# potential <- potential/cellStats(potential, 'max')
#
# potential_poly <- rasterToPolygons(poss)
# writeOGR(potential_poly, 'AGRA_TZA_output.kml', 'layer', driver='KML', 
#          overwrite=TRUE)


library(RJSONIO)
library(dplyr)

###############################################################################
### Pick variables

indicator_list <- fromJSON('http://api.dhsprogram.com/rest/dhs/indicators?APIkey=CVINTL-877527?returnFields=IndicatorId,Label,Definition')
# Unlist the JSON file entries
indicator_list <- lapply(indicator_list$Data, function(x) {unlist(x)})
# Convert JSON input to a data frame
indicator_list <- as.data.frame(do.call("rbind", indicator_list), stringsAsFactors=FALSE)

grep_list <- function(x) {
    def_rows <- grepl(tolower(x), tolower(indicator_list$Label))
    label_rows <- grepl(tolower(x), tolower(indicator_list$Definition))
    return(indicator_list[(def_rows | label_rows), 1:3])
}

###############################################################################
###  Build indicator

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

unique(dhs_vars$Indicator)

dhs_vars$SurveyYear <- as.numeric(dhs_vars$SurveyYear)
dhs_vars$Value <- as.numeric(dhs_vars$Value)

names(dhs_vars)[names(dhs_vars) == "RegionId"] <- 'REG_ID'

# Filter to only include most recent data
dhs_vars <- dhs_vars[dhs_vars$SurveyYear == max(dhs_vars$SurveyYear), ]

#ggplot(dhs_vars) + geom_point(aes(Indicator, Value))

# TODO: now join Tanzania polygons
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
