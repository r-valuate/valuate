library(testthat)
library(sf)
library(terra)

# Datos sinteticos y pequenos para que el test sea rapido:
# un area circular de 1 km de radio y un raster aleatorio que la cubre.
crear_datos_raster <- function() {
  p <- sf::st_point(c(-58.4173, -34.6118))
  area <- sf::st_sf(geometry = sf::st_buffer(sf::st_sfc(p, crs = 4326), dist = 1000))
  bb <- sf::st_bbox(sf::st_transform(area, 3857))
  original <- terra::rast(xmin = bb[["xmin"]] - 500, xmax = bb[["xmax"]] + 500,
                          ymin = bb[["ymin"]] - 500, ymax = bb[["ymax"]] + 500,
                          resolution = 20, crs = "epsg:3857")
  set.seed(1)
  terra::values(original) <- stats::runif(terra::ncell(original))
  list(area = area, original = original)
}

test_that("calcular_raster genera un .tif con la resolucion y el CRS pedidos", {
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  datos <- crear_datos_raster()
  nombre <- "test_calcular_raster"

  expect_message(
    calcular_raster(datos$original, datos$area, dim = 100, entorno = 0, nombre = nombre),
    "El proceso de calculo ha finalizado"
  )
  expect_true(file.exists(paste0(nombre, ".tif")))

  resultado <- terra::rast(paste0(nombre, ".tif"))
  expect_equal(terra::res(resultado), c(100, 100))
  expect_true(grepl("merc", terra::crs(resultado, proj = TRUE)))
  # Los valores originales estan entre 0 y 1, el remuestreo debe respetarlo
  rango <- terra::minmax(resultado)
  expect_gte(rango[1, 1], 0)
  expect_lte(rango[2, 1], 1)

  unlink(paste0(nombre, ".tif"))
})

test_that("calcular_raster con entorno > 0 suaviza los valores", {
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  datos <- crear_datos_raster()

  suppressMessages({
    calcular_raster(datos$original, datos$area, dim = 100, entorno = 0, nombre = "test_sin_entorno")
    calcular_raster(datos$original, datos$area, dim = 100, entorno = 200, nombre = "test_con_entorno")
  })

  sin_entorno <- terra::values(terra::rast("test_sin_entorno.tif"), na.rm = TRUE)
  con_entorno <- terra::values(terra::rast("test_con_entorno.tif"), na.rm = TRUE)

  expect_gt(length(con_entorno), 0)
  # El promedio movil reduce la variabilidad de un raster aleatorio
  expect_lt(stats::sd(con_entorno), stats::sd(sin_entorno))

  unlink(c("test_sin_entorno.tif", "test_con_entorno.tif"))
})
