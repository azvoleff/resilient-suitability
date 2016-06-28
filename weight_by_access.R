# Weight areas based on their market access
access_weights <- function(iso3='TZA') {
    in_database <- file.path(data_base, 'GROADS', 'gROADS_v1.gdb')
    # TODO: Need to buffer the spat to account for roads that may fall 
    # outside national border but still be closs enough to affect access.
    spat <- as.numeric(bbox(base))
    out_file <- tempfile(fileext='.shp')
    ogr2ogr(in_database, out_file, spat=spat, spat_srs=proj4string(base),
            simplify=res_degrees/10)
            #clipsrc=clipsrc, simplify=res_degrees/10)
    roads <- readShapeSpatial(out_file)

    plot(roads)

    # Buffer roads by 10 km
    roads <- gBuffer(roads, res_degrees/2, byid=TRUE)
}

