library(testthat)
library(sf)
library(terra)
library(fasterize)
library(viridis)

test_that("calcular_dist genera un raster con CRS correcto", {
  # Crear datos de prueba asegurando que sean objetos sf válidos
  p <- sf::st_point(c(-58.4173, -34.6118)) # Punto en Buenos Aires
  objeto <- sf::st_sf(geometry = sf::st_sfc(p, crs = 4326)) # Crear sf válido para el objeto

  area_geom <- sf::st_buffer(sf::st_sfc(p, crs = 4326), dist = 1000) # Área de prueba con buffer
  area <- sf::st_sf(geometry = area_geom) # Convertir en sf válido

  dim <- 100
  nombre <- "test_raster"
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)

  # Ejecutar la función y verificar que devuelve el raster
  expect_message(test_raster <- calcular_dist(area, objeto, dim, nombre),
                 "El proceso de calculo ha finalizado")

  # La funcion no debe crear objetos en el entorno global
  expect_false(exists(nombre, envir = globalenv(), inherits = FALSE))

  # Verificar que el objeto generado es un SpatRaster
  expect_s4_class(test_raster, "SpatRaster")

  # Obtener el CRS del raster como PROJ4 y verificar que es Mercator
  raster_crs <- terra::crs(test_raster, proj = TRUE)
  expect_true(grepl("\\+proj=merc", raster_crs))

  # Verificar que el archivo .tif fue creado
  expect_true(file.exists(paste0(nombre, ".tif")))

  # Limpiar archivos de prueba
  unlink(paste0(nombre, ".tif"))
})

test_that("calcular_dist maneja entradas incorrectas", {
  p <- sf::st_point(c(-58.4173, -34.6118))
  area_geom <- sf::st_buffer(sf::st_sfc(p, crs = 4326), dist = 1000)
  area <- sf::st_sf(geometry = area_geom)

  # Objeto inválido: en vez de NULL, pasamos un data.frame vacío
  objeto_invalido <- data.frame()

  dim <- 100
  nombre <- "test_raster"

  expect_error(calcular_dist(area, objeto_invalido, dim, nombre), "st_transform")
})

