library(testthat)

test_that("crear_dummies agrega una columna 0/1 por categoria de cada factor", {
  datos <- data.frame(id = 1:4, superficie = c(1.5, 2, 3, 4),
                      situacion = factor(c("b", "a", "b", "c"), levels = c("b", "a", "c")))
  res <- valuate:::crear_dummies(datos)

  expect_equal(names(res), c("id", "superficie", "situacion", "situacionb", "situaciona", "situacionc"))
  expect_equal(res$situacionb, c(1L, 0L, 1L, 0L))
  expect_equal(res$situaciona, c(0L, 1L, 0L, 0L))
  expect_equal(res$situacionc, c(0L, 0L, 0L, 1L))
  # Las columnas numericas no se modifican
  expect_equal(res$superficie, datos$superficie)
})

test_that("crear_dummies marca los faltantes y agrega una columna para NA", {
  datos <- data.frame(f = factor(c("a", NA, "b")), stringsAsFactors = FALSE)
  res <- valuate:::crear_dummies(datos)

  expect_equal(names(res), c("f", "fa", "fb", "fNA"))
  expect_equal(res$fa, c(1L, NA, 0L))
  expect_equal(res$fNA, c(0L, 1L, 0L))
})

test_that("crear_dummies ordena las categorias de texto de forma natural", {
  datos <- data.frame(lote = c("b10", "b2", NA, "b2"), stringsAsFactors = FALSE)
  res <- valuate:::crear_dummies(datos)

  expect_equal(names(res), c("lote", "loteb2", "loteb10", "loteNA"))
  expect_equal(res$loteb2, c(0L, 1L, NA, 1L))
  expect_equal(res$loteNA, c(0L, 0L, 1L, 0L))
})

test_that("crear_dummies conserva la geometria de los objetos sf", {
  datos <- sf::st_sf(f = factor(c("a", "b")),
                     geom = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3857))
  res <- valuate:::crear_dummies(datos)

  expect_s3_class(res, "sf")
  expect_equal(sf::st_drop_geometry(res)$fa, c(1L, 0L))
})
