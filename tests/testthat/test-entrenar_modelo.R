library(testthat)
library(sf)
library(terra)

# Rasters chicos (recortes de los datos de Medellin) incluidos en tests/testthat/fixtures
rasters_de_prueba <- function() {
  normalizePath(test_path("fixtures", c("dist_tren.tif", "dist_vias_prim.tif", "entorno_comercios.tif")))
}

test_that("entrenar_modelo devuelve la prediccion, el modelo y las muestras resultantes", {
  independientes <- rasters_de_prueba()
  carpeta <- tempfile("entrenar_modelo")
  dir.create(carpeta)
  old <- setwd(carpeta)
  on.exit({
    setwd(old)
    unlink(carpeta, recursive = TRUE)
  }, add = TRUE)

  data("datos", package = "valuate", envir = environment())
  muestra <- dat[1:150, ]

  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Modelo lineal y umbral alto: una sola iteracion, para que el test sea rapido
  expect_message(
    salida <- entrenar_modelo(df = muestra, dependiente = "vut", independientes = independientes,
                              modelo = "lm", umbral = 10),
    "El entrenamiento ha finalizado"
  )

  expect_named(salida, c("prediccion", "modelo", "utilizados", "eliminados"))
  expect_false(exists("datos_utilizados", envir = globalenv(), inherits = FALSE))

  expect_s4_class(salida$prediccion, "SpatRaster")
  expect_equal(as.vector(terra::ext(salida$prediccion)),
               as.vector(terra::ext(terra::rast(independientes[1]))))
  expect_s3_class(salida$modelo, "train")

  expect_s3_class(salida$utilizados, "sf")
  expect_lte(nrow(salida$utilizados) + nrow(salida$eliminados), nrow(muestra))
  expect_true(all(c("dist_tren", "dist_vias_prim", "entorno_comercios") %in% names(salida$utilizados)))

  # La prediccion y el modelo tambien se guardan en el directorio de trabajo
  expect_true(file.exists("vut.tif"))
  expect_true(file.exists("modelo.rda"))
})
