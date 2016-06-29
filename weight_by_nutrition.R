###############################################################################
### Nutrition
###############################################################################

# Function of nutrition indicators from DHS
ciaf_weights <- function(iso3='TZA') {

}


# First go:
potential <- mask(poss, get_country_poly())
potential <- potential/cellStats(potential, 'max')

potential_poly <- rasterToPolygons(poss)
writeOGR(potential_poly, 'AGRA_TZA_output.kml', 'layer', driver='KML', 
         overwrite=TRUE)

