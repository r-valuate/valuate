library(testthat)
library(sf)

test_that("eliminar_outliers (sin Moran) devuelve la muestra sin observaciones atipicas", {
  data("resultado", package = "valuate", envir = environment())
  muestra <- resultado[1:300, ]
  n_validos <- sum(!is.na(muestra$vut))

  expect_message(
    depurados <- eliminar_outliers(df = muestra, dist = 500, variable = "vut",
                                   umbral = 0.3, moran = FALSE),
    "Se eliminaron"
  )
  expect_false(exists("depurados", envir = globalenv(), inherits = FALSE))
  expect_s3_class(depurados, "sf")
  expect_lte(nrow(depurados), n_validos)
  expect_false(anyNA(depurados$vut))
  expect_false("drop" %in% names(depurados))
})

test_that("un umbral mas exigente elimina al menos tantas observaciones", {
  data("resultado", package = "valuate", envir = environment())
  muestra <- resultado[1:300, ]

  laxo <- suppressMessages(eliminar_outliers(muestra, dist = 500, variable = "vut", umbral = 0.5, moran = FALSE))
  estricto <- suppressMessages(eliminar_outliers(muestra, dist = 500, variable = "vut", umbral = 0.1, moran = FALSE))

  expect_lte(nrow(estricto), nrow(laxo))
})

test_that("eliminar_outliers con Moran local devuelve la muestra depurada", {
  data("resultado", package = "valuate", envir = environment())
  muestra <- resultado[1:300, ]

  expect_message(
    depurados <- eliminar_outliers(df = muestra, dist = 500, variable = "vut", moran = TRUE),
    "indice de Moran local"
  )
  expect_s3_class(depurados, "sf")
  expect_lte(nrow(depurados), sum(!is.na(muestra$vut)))
  expect_false("drop" %in% names(depurados))
})
