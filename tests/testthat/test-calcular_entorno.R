library(testthat)
library(sf)
library(terra)
library(fasterize)
library(dplyr)
library(viridis)

test_that("calcular_entorno genera un raster con CRS correcto", {
  # Crear datos de prueba asegurando que sean objetos sf válidos
  p <- sf::st_point(c(-58.4173, -34.6118)) # Punto en Buenos Aires
  objeto <- sf::st_sf(geometry = sf::st_sfc(p, crs = 4326)) # Crear sf válido para el objeto

  area_geom <- sf::st_buffer(sf::st_sfc(p, crs = 4326), dist = 1000) # Área de prueba con buffer
  area <- sf::st_sf(geometry = area_geom) # Convertir en sf válido

  dim <- 100
  ext <- 200
  nombre <- "test_entorno"

  # Ejecutar la función y verificar que genera la salida esperada
  expect_output(calcular_entorno(area, objeto, dim, ext, nombre), "El proceso de cálculo ha finalizado")

  # Verificar que la variable se creó en el entorno global
  expect_true(exists(nombre, envir = globalenv()))

  # Obtener el raster generado
  test_raster <- get(nombre, envir = globalenv())

  # Verificar que el objeto generado es un SpatRaster
  expect_s4_class(test_raster, "SpatRaster")

  # Verificar que el CRS del raster contiene "merc" y "units=m" (EPSG:3857)
  crs_actual <- terra::crs(test_raster, proj=TRUE)
  expect_true(grepl("merc", crs_actual) && grepl("units=m", crs_actual),
              info = paste("CRS obtenido:", crs_actual))

  # Verificar que el archivo .tif fue creado
  expect_true(file.exists(paste0(nombre, ".tif")))

  # Limpiar archivos de prueba
  unlink(paste0(nombre, ".tif"))
  rm(list = nombre, envir = globalenv())
})

test_that("calcular_entorno maneja geometrías no válidas", {
  p <- sf::st_point(c(-58.4173, -34.6118))
  area_geom <- sf::st_buffer(sf::st_sfc(p, crs = 4326), dist = 1000)
  area <- sf::st_sf(geometry = area_geom)

  # Crear objeto con una geometría no válida (LINESTRING)
  objeto_invalido <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(matrix(c(-58.417, -34.611, -58.418, -34.612), ncol=2)), crs = 4326))

  dim <- 100
  ext <- 200
  nombre <- "test_entorno"

  expect_error(calcular_entorno(area, objeto_invalido, dim, ext, nombre),
               "Error: El objeto provisto para el cálculo debe tener geometrías de clase 'POINT', 'POLYGON' o 'MULTIPOLYGON'.")
})
