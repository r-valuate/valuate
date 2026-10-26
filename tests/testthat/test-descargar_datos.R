library(testthat)

test_that("datos_disponibles lista los rasters de ejemplo", {
  disponibles <- datos_disponibles()
  expect_type(disponibles, "character")
  expect_true(all(grepl("\\.tif$", disponibles)))
  expect_true("dist_tren.tif" %in% disponibles)
})

test_that("descargar_datos rechaza archivos que no existen", {
  expect_error(descargar_datos("no_existe.tif", directorio = tempfile()), "no estan disponibles")
})

test_that("descargar_datos descarga los archivos faltantes y reutiliza los existentes", {
  # Se simula el servidor con una carpeta local, para no depender de internet
  origen <- normalizePath(test_path("fixtures"), winslash = "/")
  old <- options(valuate.url_datos = paste0("file:///", sub("^/", "", origen), "/"))
  on.exit(options(old), add = TRUE)
  destino <- tempfile("cache")

  expect_message(
    rutas <- descargar_datos(c("dist_tren.tif", "entorno_comercios.tif"), directorio = destino),
    "Descargando"
  )
  expect_named(rutas, c("dist_tren.tif", "entorno_comercios.tif"))
  expect_true(all(file.exists(rutas)))
  expect_equal(unname(tools::md5sum(rutas[1])),
               unname(tools::md5sum(file.path(origen, "dist_tren.tif"))))

  # La segunda vez no se descarga nada
  expect_no_message(descargar_datos("dist_tren.tif", directorio = destino))
})

test_that("descargar_datos informa los errores de descarga", {
  old <- options(valuate.url_datos = "file:///carpeta/que/no/existe/")
  on.exit(options(old), add = TRUE)
  destino <- tempfile("cache")

  expect_error(
    suppressWarnings(suppressMessages(descargar_datos("dist_tren.tif", directorio = destino))),
    "No se pudo descargar"
  )
  expect_false(file.exists(file.path(destino, "dist_tren.tif")))
})
