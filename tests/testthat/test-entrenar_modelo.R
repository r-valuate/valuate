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

test_that("entrenar_modelo elimina las observaciones con error alto y vuelve a entrenar", {
  independientes <- rasters_de_prueba()
  carpeta <- tempfile("entrenar_modelo")
  dir.create(carpeta)
  old <- setwd(carpeta)
  on.exit({
    setwd(old)
    unlink(carpeta, recursive = TRUE)
  }, add = TRUE)
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  data("datos", package = "valuate", envir = environment())
  muestra <- dat[1:150, ]

  # Con un modelo lineal el MAPE inicial es cercano a 73%: con umbral = 0.6
  # se eliminan las observaciones con error >= 40% y se entrena de nuevo.
  mensajes <- character(0)
  salida <- withCallingHandlers(
    entrenar_modelo(df = muestra, dependiente = "vut", independientes = independientes,
                    modelo = "lm", umbral = 0.6),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    })

  expect_true(any(grepl("Se eliminaron [1-9][0-9]* observaciones", mensajes)))
  expect_true(any(grepl("El entrenamiento ha finalizado", mensajes)))
  expect_gt(nrow(salida$eliminados), 0)
  expect_gt(nrow(salida$utilizados), 0)

  # El modelo final cumple el umbral con las observaciones que quedaron
  final <- salida$modelo$pred
  expect_lt(mean(abs(final$pred - final$obs) / final$obs), 0.6)
  expect_equal(nrow(final), nrow(salida$utilizados))
})

test_that("entrenar_modelo se detiene si no quedan observaciones para eliminar", {
  independientes <- rasters_de_prueba()
  carpeta <- tempfile("entrenar_modelo")
  dir.create(carpeta)
  old <- setwd(carpeta)
  on.exit({
    setwd(old)
    unlink(carpeta, recursive = TRUE)
  }, add = TRUE)
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  data("datos", package = "valuate", envir = environment())
  muestra <- dat[1:150, ]

  # Umbral imposible de alcanzar y ninguna observacion con error >= 100 (10.000%)
  expect_message(
    salida <- suppressWarnings(
      entrenar_modelo(df = muestra, dependiente = "vut", independientes = independientes,
                      modelo = "lm", umbral = 0.1, eliminar = 100)
    ),
    "El proceso se detiene"
  )
  expect_equal(nrow(salida$eliminados), 0)
})

test_that("nucleos_a_usar usa un solo nucleo en GitHub Actions", {
  valor_original <- Sys.getenv("GITHUB_ACTIONS", unset = NA)
  on.exit({
    if (is.na(valor_original)) Sys.unsetenv("GITHUB_ACTIONS") else Sys.setenv(GITHUB_ACTIONS = valor_original)
  }, add = TRUE)

  Sys.setenv(GITHUB_ACTIONS = "true")
  expect_equal(valuate:::nucleos_a_usar(), 1L)

  Sys.setenv(GITHUB_ACTIONS = "false")
  expect_equal(valuate:::nucleos_a_usar(), parallel::detectCores())
})
