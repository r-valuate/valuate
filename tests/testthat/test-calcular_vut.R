library(testthat)
library(sf)

test_that("calcular_vut agrega las columnas 'vut' y 'coef' y devuelve una tabla de efectos", {
  data("terrenos", package = "valuate", envir = environment())
  muestra <- terrenos[1:250, ]

  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  tabla <- suppressMessages(
    calcular_vut(df = muestra,
                 index = "id",
                 independientes = c("tipo_valor" = "Venta",
                                    "superficie" = "mediana",
                                    "situacion_juridica" = "Con Escritura"),
                 dependiente = "valor_m2",
                 dist = 500)
  )

  expect_s3_class(tabla, "knitr_kable")
  expect_true(exists("resultado", envir = globalenv()))

  res <- get("resultado", envir = globalenv())
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), nrow(muestra))
  expect_true(all(c("vut", "coef") %in% names(res)))
  # vut = valor_m2 * coef
  expect_equal(res$vut, res$valor_m2 * res$coef)
  expect_true(all(res$vut[!is.na(res$vut)] > 0))

  rm("resultado", envir = globalenv())
})

test_that("calcular_vut informa la eliminacion de datos faltantes", {
  data("terrenos", package = "valuate", envir = environment())
  muestra <- terrenos[1:250, ]
  muestra$superficie[1:3] <- NA

  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_message(
    calcular_vut(df = muestra, index = "id",
                 independientes = c("tipo_valor" = "Venta",
                                    "superficie" = "mediana",
                                    "situacion_juridica" = "Con Escritura"),
                 dependiente = "valor_m2", dist = 500),
    "Se eliminaran 3 observaciones con datos faltantes"
  )
  rm("resultado", envir = globalenv())
})
