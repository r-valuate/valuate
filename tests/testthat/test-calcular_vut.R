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

test_that("calcular_vut funciona cuando no hay dependencia espacial (modelo MCO)", {
  # Datos simulados con ubicaciones al azar: no hay dependencia espacial,
  # por lo que la funcion debe usar el modelo de minimos cuadrados ordinarios.
  # Efectos verdaderos: superficie = -0.4, "Sin escritura" = -0.5
  set.seed(1)
  n <- 200
  puntos <- sf::st_sfc(lapply(seq_len(n), function(i) {
    sf::st_point(c(stats::runif(1, 0, 5000), stats::runif(1, 0, 5000)))
  }), crs = 3857)
  situacion <- sample(c("Con Escritura", "Sin escritura"), n, replace = TRUE, prob = c(0.7, 0.3))
  superficie <- round(exp(stats::rnorm(n, log(500), 0.4)))
  valor_m2 <- exp(5 - 0.4 * log(superficie / 500) - 0.5 * (situacion == "Sin escritura") +
                    stats::rnorm(n, 0, 0.2))
  muestra <- sf::st_sf(id = seq_len(n), superficie = superficie,
                       situacion_juridica = situacion, valor_m2 = valor_m2, geom = puntos)

  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_output(
    tabla <- suppressMessages(
      calcular_vut(df = muestra, index = "id",
                   independientes = c("superficie" = "mediana",
                                      "situacion_juridica" = "Con Escritura"),
                   dependiente = "valor_m2", dist = 300)
    ),
    "minimos cuadrados ordinarios"
  )
  expect_s3_class(tabla, "knitr_kable")

  res <- get("resultado", envir = globalenv())
  expect_true(all(c("vut", "coef") %in% names(res)))
  expect_equal(res$vut, res$valor_m2 * res$coef)
  # El lote tipico (mediana de superficie, con escritura) tiene coeficiente cercano a 1
  expect_equal(stats::median(res$coef[res$situacion_juridica == "Con Escritura"]), 1, tolerance = 0.05)

  rm("resultado", envir = globalenv())
})
