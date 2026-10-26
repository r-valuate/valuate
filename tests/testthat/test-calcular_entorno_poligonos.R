library(testthat)
library(sf)
library(terra)

test_that("calcular_entorno con poligonos devuelve la proporcion cubierta (entre 0 y 1)", {
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)

  p <- sf::st_point(c(-58.4173, -34.6118))
  area <- sf::st_sf(geometry = sf::st_buffer(sf::st_sfc(p, crs = 4326), dist = 1000))

  # Una "plaza" cuadrada de 400 x 400 metros en el centro del area
  centro <- sf::st_transform(sf::st_sfc(p, crs = 4326), 3857)
  plaza <- sf::st_sf(geometry = sf::st_buffer(centro, dist = 200, endCapStyle = "SQUARE"))

  nombre <- "test_entorno_poligonos"
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_message(
    resultado <- calcular_entorno(area, plaza, dim = 100, ext = 200, nombre = nombre),
    "El proceso de calculo ha finalizado"
  )
  expect_true(file.exists(paste0(nombre, ".tif")))
  expect_s4_class(resultado, "SpatRaster")
  valores <- terra::values(resultado, na.rm = TRUE)
  expect_gte(min(valores), 0)
  expect_lte(max(valores), 1 + 1e-8)
  # Cerca de la plaza la proporcion es alta y lejos es cero
  expect_gt(max(valores), 0.5)
  expect_equal(min(valores), 0)

  unlink(paste0(nombre, ".tif"))
})
