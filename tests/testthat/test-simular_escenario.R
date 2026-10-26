library(testthat)
library(sf)
library(terra)

test_that("simular_escenario estima el impacto de un cambio en una variable", {
  archivos <- c("dist_tren.tif", "dist_vias_prim.tif", "entorno_comercios.tif")
  originales <- normalizePath(test_path("fixtures", archivos))
  carpeta <- tempfile("simular_escenario")
  dir.create(file.path(carpeta, "escenario"), recursive = TRUE)
  old <- setwd(carpeta)
  on.exit({
    setwd(old)
    unlink(carpeta, recursive = TRUE)
  }, add = TRUE)

  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # 1. Entrenamos un modelo chico, igual que en test-entrenar_modelo.R
  data("datos", package = "valuate", envir = environment())
  suppressMessages(
    entrenar_modelo(df = dat[1:150, ], dependiente = "vut", independientes = originales,
                    modelo = "lm", umbral = 10)
  )

  # 2. Escenario: desaparecen los comercios en un tercio del area.
  #    Los archivos simulados tienen los mismos nombres que los originales.
  simulados <- file.path(carpeta, "escenario", archivos)
  file.copy(originales[1:2], simulados[1:2])
  comercios <- terra::rast(originales[3])
  valores <- terra::values(comercios)
  valores[seq_len(length(valores) %/% 3)] <- 0
  terra::values(comercios) <- valores
  terra::writeRaster(comercios, simulados[3], overwrite = TRUE)

  # 3. Simulamos
  expect_message(
    impacto <- simular_escenario(modelo = "modelo.rda", independientes = simulados,
                                 original = "vut.tif"),
    "impacto.tif"
  )
  expect_s4_class(impacto, "SpatRaster")
  expect_gt(sum(!is.na(terra::values(impacto))), 0)
  expect_true(file.exists("simulacion.tif"))
  expect_true(file.exists("impacto.tif"))
})
