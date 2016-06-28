library(httr)
library(maptools)

run_q <- function(q) {
    GET(paste0("https://cdb.resilienceatlas.org/user/ra/api/v2/sql?q=", q))
}

r <- run_q("SELECT ST_Value(the_raster_webmercator, 'SRID=3857;POINT(-10 10)'::geometry) As b1pval FROM global_cru_ts3_23_tmx_19850101_20141231_trend_decadal WHERE ST_Intersects(the_raster_webmercator, 'SRID=3857;POINT(-10 10)'::geometry)")

# Load TZA shape
r

r <- GET('https://cdb.resilienceatlas.org/user/ra/api/v2/sql?q=SELECT ST_AsText(the_geom) from gadm28_adm0 where iso = "TZA"')

content(r)

data.frame(content(r))
