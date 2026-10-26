library(testthat)
library(sf)

test_that("calcular_vut devuelve la muestra con 'vut' y 'coef' y la tabla de efectos", {
  data("terrenos", package = "valuate", envir = environment())
  muestra <- terrenos[1:250, ]

  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  capture.output(salida <- suppressMessages(
    calcular_vut(df = muestra,
                 index = "id",
                 independientes = c("tipo_valor" = "Venta",
                                    "superficie" = "mediana",
                                    "situacion_juridica" = "Con Escritura"),
                 dependiente = "valor_m2",
                 dist = 500)
  ))

  expect_type(salida, "list")
  expect_named(salida, c("datos", "efectos"))
  expect_false(exists("resultado", envir = globalenv(), inherits = FALSE))
  expect_s3_class(salida$efectos, "data.frame")
  expect_named(salida$efectos, c("Variable", "p-valor", "Efecto"))

  res <- salida$datos
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), nrow(muestra))
  expect_true(all(c("vut", "coef") %in% names(res)))
  # vut = valor_m2 * coef
  expect_equal(res$vut, res$valor_m2 * res$coef)
  expect_true(all(res$vut[!is.na(res$vut)] > 0))
})

test_that("calcular_vut informa la eliminacion de datos faltantes", {
  data("terrenos", package = "valuate", envir = environment())
  muestra <- terrenos[1:250, ]
  muestra$superficie[1:3] <- NA

  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_message(
    capture.output(calcular_vut(df = muestra, index = "id",
                 independientes = c("tipo_valor" = "Venta",
                                    "superficie" = "mediana",
                                    "situacion_juridica" = "Con Escritura"),
                 dependiente = "valor_m2", dist = 500)),
    "Se eliminaran 3 observaciones con datos faltantes"
  )
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
    salida <- suppressMessages(
      calcular_vut(df = muestra, index = "id",
                   independientes = c("superficie" = "mediana",
                                      "situacion_juridica" = "Con Escritura"),
                   dependiente = "valor_m2", dist = 300)
    ),
    "minimos cuadrados ordinarios"
  )
  expect_named(salida, c("datos", "efectos"))
  # Recupera los efectos verdaderos
  efectos <- stats::setNames(salida$efectos$Efecto, salida$efectos$Variable)
  expect_equal(unname(efectos["superficie"]), -0.4, tolerance = 0.15)
  expect_equal(unname(efectos["situacion_juridicaSin escritura"]), -0.5, tolerance = 0.15)

  res <- salida$datos
  expect_true(all(c("vut", "coef") %in% names(res)))
  expect_equal(res$vut, res$valor_m2 * res$coef)
  # El lote tipico (mediana de superficie, con escritura) tiene coeficiente cercano a 1
  expect_equal(stats::median(res$coef[res$situacion_juridica == "Con Escritura"]), 1, tolerance = 0.05)
})

test_that("calcular_vut aplica un modelo LAG cuando corresponde", {
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  res <- correr_calcular_vut(simular_dependencia(53, rho = 0.1, tipo = "lag", dist = 500), dist = 500)

  expect_true(any(grepl("modelo de rezago espacial LAG", res$mensajes)))
  efectos <- stats::setNames(res$salida$efectos$Efecto, res$salida$efectos$Variable)
  expect_equal(unname(efectos["superficie"]), -0.4, tolerance = 0.15)
  expect_equal(unname(efectos["situacion_juridicaSin escritura"]), -0.5, tolerance = 0.15)
  expect_equal(res$salida$datos$vut, res$salida$datos$valor_m2 * res$salida$datos$coef)
})

test_that("calcular_vut aplica un modelo de ERROR espacial cuando corresponde", {
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  res <- correr_calcular_vut(simular_dependencia(108, rho = 0.2, tipo = "error", dist = 300), dist = 300)

  expect_true(any(grepl("rezago del ERROR espacial", res$mensajes)))
  efectos <- stats::setNames(res$salida$efectos$Efecto, res$salida$efectos$Variable)
  expect_equal(unname(efectos["superficie"]), -0.4, tolerance = 0.15)
  expect_equal(unname(efectos["situacion_juridicaSin escritura"]), -0.5, tolerance = 0.15)
  expect_equal(res$salida$datos$vut, res$salida$datos$valor_m2 * res$salida$datos$coef)
})

test_that("calcular_vut puede usar la media como referencia del lote tipico", {
  data("terrenos", package = "valuate", envir = environment())
  muestra <- terrenos[1:250, ]
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  correr <- function(referencia) {
    salida <- NULL
    capture.output(salida <- suppressMessages(calcular_vut(
      df = muestra, index = "id", dependiente = "valor_m2",
      independientes = c("superficie" = referencia, "situacion_juridica" = "Con Escritura"),
      dist = 500)))
    salida$datos
  }
  con_media <- correr("media")
  con_mediana <- correr("mediana")

  expect_equal(con_media$vut, con_media$valor_m2 * con_media$coef)
  # Cambiar la referencia del lote tipico cambia los coeficientes
  expect_false(isTRUE(all.equal(con_media$coef, con_mediana$coef)))
})

test_that("calcular_vut informa y descarta las geometrias duplicadas", {
  data("terrenos", package = "valuate", envir = environment())
  muestra <- terrenos[1:250, ]
  muestra <- rbind(muestra, muestra[1:3, ])
  muestra$id[251:253] <- max(muestra$id) + 1:3
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_message(
    capture.output(calcular_vut(df = muestra, index = "id", dependiente = "valor_m2",
                                independientes = c("tipo_valor" = "Venta",
                                                   "superficie" = "mediana",
                                                   "situacion_juridica" = "Con Escritura"),
                                dist = 500)),
    "Se eliminaran 3 observaciones con geometrias duplicadas"
  )
})
