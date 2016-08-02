library(httr)
library(rgeos)
library(maptools)
library(raster)
library(gdalUtils)
library(rgdal)
library(tools)
library(dplyr)
library(tidyr)
library(gfcanalysis)

#data_base <- 'H:/Data'
data_base <- 'O:/Data'
iso3 <- 'TZA'
res_degrees <- .08333333
clipsrc <- file.path(data_base, 'Global', 'GADM', 'TZA_adm0.shp')

# Function of population, road access
get_country_poly <- function(iso3='TZA') {
    # TODO: Code to pull proper country based on iso3='TZA', hardcoded now for speed
    readOGR(file.path(data_base, 'Global', 'GADM'), 'TZA_adm0')
}

setup_base_layer <- function(iso3='TZA') {
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

save_raster(x, name) {
    writeRaster(v, filename=paste0(x, '.tif'), overwrite=TRUE)
    v <- polygonize(x)
    writeOGR(v, paste0(x, '.kml'), 'layer', driver='KML', overwrite=TRUE)
}
