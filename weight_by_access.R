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
    rp <- intersect(roads, rsp)
    rp$length <- gLength(rp, byid=TRUE) / 1000
    x <- tapply(rp$length, rp$layer, sum)
    r <- raster(rs)
    r[as.integer(names(x))] <- x
    # Density is in km/km^2

    road_density <- projectRaster(r, crs=proj4string(base))
    road_density_poly <- polygonize(road_density)
    writeOGR(road_density_poly, 'AGRA_TZA_groads_density.kml', 'groads', driver='KML', 
             overwrite=TRUE)

    # Compute population density per grid square
    pop <- raster(file.path(data_base, 'Landscan', 'Landscan2014', 'landscan_2014.tif'))
    pop <- crop(pop, get_country_poly())
    writeRaster(pop, 'AGRA_TZA_pop_density_original.tif', overwrite=TRUE)

    pop <- aggregate(pop, fact=c(round(res(base)/res(pop))), expand=FALSE, 
                     fun=sum)
    # Convert to pop/km^2
    pop <- pop/(10*10)
    pop_poly <- polygonize(pop)
    writeOGR(pop_poly, 'AGRA_TZA_pop_density.kml', 'pop', driver='KML', 
             overwrite=TRUE)
}
