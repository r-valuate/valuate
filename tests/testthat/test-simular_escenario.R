library(testthat)
library(sf)
library(terra)

test_that("simular_escenario estima el impacto de un cambio en una variable", {
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
  muestra <- dat[1:150, ]
  e <- terra::ext(terra::vect(muestra)) + 300
  archivos <- c("dist_tren.tif", "dist_vias_prim.tif", "entorno_comercios.tif")
  originales <- file.path(carpeta, archivos)
  for (i in seq_along(archivos)) {
    r <- terra::rast(system.file("extdata", archivos[i], package = "valuate"))
    terra::writeRaster(terra::aggregate(terra::crop(r, e), fact = 5), originales[i], overwrite = TRUE)
  }
  suppressMessages(
    entrenar_modelo(df = muestra, dependiente = "vut", independientes = originales,
                    modelo = "lm", umbral = 10)
  )
  rm("datos_utilizados", "datos_eliminados", envir = globalenv())

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
    simular_escenario(modelo = "modelo.rda", independientes = simulados, original = "vut.tif"),
    "impacto.tif"
  )
  expect_true(file.exists("simulacion.tif"))
  expect_true(file.exists("impacto.tif"))

  impacto <- terra::rast("impacto.tif")
  expect_s4_class(impacto, "SpatRaster")
  expect_gt(sum(!is.na(terra::values(impacto))), 0)
})
