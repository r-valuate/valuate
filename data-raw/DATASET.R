original = terra::rast("data-raw/edificaciones.tif")
entorno = 100
nombre = "prueba_raster"

rm(list=ls())
#
library(sf)
original = terra::rast("data/edificaciones.tif")
area_de_estudio = sf::st_read("data/area.gpkg")
calcular_raster(original, area_de_estudio, dim = 50, entorno = 100, "eliminar")
