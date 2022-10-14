# Create sf objects from Lat lon data,
#import of Borders of state and nature  from gpkg 
#Namen cleanen von Naturraum etc , Alte Pheno skripts anschauen

library(sf)
gfile <- system.file("data/Naturraum_Grenzen_DE.gpkg", package = "sf", mustWork = TRUE)
gfile <- "data/Naturraum_Grenzen_DE.gpkg"
sf::st_layers(gfile)
read_sf("data/Naturraum_Grenzen_DE.gpkg")
