library(testthat)
library(sf)
library(terra)

# Recorta y agrega algunos rasters del paquete alrededor de una muestra chica
# de puntos, para que el entrenamiento y la interpolacion sean rapidos.
preparar_rasters <- function(puntos, carpeta) {
  e <- terra::ext(terra::vect(puntos)) + 300
  archivos <- c("dist_tren.tif", "dist_vias_prim.tif", "entorno_comercios.tif")
  vapply(archivos, function(f) {
    r <- terra::rast(system.file("extdata", f, package = "valuate"))
    r <- terra::aggregate(terra::crop(r, e), fact = 5)
    salida <- file.path(carpeta, f)
    terra::writeRaster(r, salida, overwrite = TRUE)
    salida
  }, character(1), USE.NAMES = FALSE)
}

test_that("entrenar_modelo genera la prediccion, el modelo y las muestras resultantes", {
  carpeta <- tempfile("entrenar_modelo")
  dir.create(carpeta)
  old <- setwd(carpeta)
  on.exit({
    setwd(old)
    unlink(carpeta, recursive = TRUE)
  }, add = TRUE)

  data("datos", package = "valuate", envir = environment())
  muestra <- dat[1:150, ]
  independientes <- preparar_rasters(muestra, carpeta)

  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Modelo lineal y umbral alto: una sola iteracion, para que el test sea rapido
  expect_message(
    entrenar_modelo(df = muestra, dependiente = "vut", independientes = independientes,
                    modelo = "lm", umbral = 10),
    "El entrenamiento ha finalizado"
  )

  expect_true(file.exists("vut.tif"))
  expect_true(file.exists("modelo.rda"))

  prediccion <- terra::rast("vut.tif")
  expect_equal(as.vector(terra::ext(prediccion)),
               as.vector(terra::ext(terra::rast(independientes[1]))))

  utilizados <- get("datos_utilizados", envir = globalenv())
  eliminados <- get("datos_eliminados", envir = globalenv())
  expect_s3_class(utilizados, "sf")
  expect_lte(nrow(utilizados) + nrow(eliminados), nrow(muestra))
  expect_true(all(basename(gsub(".tif", "", independientes)) %in% names(utilizados)))

  rm("datos_utilizados", "datos_eliminados", envir = globalenv())
})
