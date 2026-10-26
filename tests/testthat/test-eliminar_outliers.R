library(testthat)
library(sf)

test_that("eliminar_outliers (sin Moran) crea 'depurados' sin agregar observaciones", {
  data("resultado", package = "valuate", envir = environment())
  muestra <- resultado[1:300, ]
  n_validos <- sum(!is.na(muestra$vut))

  expect_output(
    eliminar_outliers(df = muestra, dist = 500, variable = "vut", umbral = 0.3, moran = FALSE),
    "Se eliminaron"
  )
  expect_true(exists("depurados", envir = globalenv()))

  depurados <- get("depurados", envir = globalenv())
  expect_s3_class(depurados, "sf")
  expect_lte(nrow(depurados), n_validos)
  expect_false(anyNA(depurados$vut))
  expect_false("drop" %in% names(depurados))

  rm("depurados", envir = globalenv())
})

test_that("un umbral mas exigente elimina al menos tantas observaciones", {
  data("resultado", package = "valuate", envir = environment())
  muestra <- resultado[1:300, ]

  capture.output(eliminar_outliers(muestra, dist = 500, variable = "vut", umbral = 0.5, moran = FALSE))
  n_laxo <- nrow(get("depurados", envir = globalenv()))
  capture.output(eliminar_outliers(muestra, dist = 500, variable = "vut", umbral = 0.1, moran = FALSE))
  n_estricto <- nrow(get("depurados", envir = globalenv()))

  expect_lte(n_estricto, n_laxo)
  rm("depurados", envir = globalenv())
})

test_that("eliminar_outliers con Moran local crea 'depurados'", {
  data("resultado", package = "valuate", envir = environment())
  muestra <- resultado[1:300, ]
  if (exists("depurados", envir = globalenv())) rm("depurados", envir = globalenv())

  expect_output(
    eliminar_outliers(df = muestra, dist = 500, variable = "vut", moran = TRUE),
    "indice de Moran local"
  )
  expect_true(exists("depurados", envir = globalenv()))

  depurados <- get("depurados", envir = globalenv())
  expect_s3_class(depurados, "sf")
  expect_lte(nrow(depurados), sum(!is.na(muestra$vut)))
  expect_false("drop" %in% names(depurados))

  rm("depurados", envir = globalenv())
})
