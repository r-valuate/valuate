# Simula una muestra de terrenos con dependencia espacial de tipo "lag" o "error".
# Efectos verdaderos: superficie = -0.4 y "Sin escritura" = -0.5 (en logaritmos).
simular_dependencia <- function(seed, rho, tipo, dist, n = 200) {
  set.seed(seed)
  xy <- cbind(stats::runif(n, 0, 5000), stats::runif(n, 0, 5000))
  vecinos <- suppressWarnings(spdep::dnearneigh(xy, 0, dist))
  distancias <- spdep::nbdists(vecinos, xy)
  lw <- spdep::nb2listw(vecinos, glist = lapply(distancias, function(x) 1 / x),
                        style = "W", zero.policy = TRUE)
  W <- spdep::listw2mat(lw)
  superficie <- round(exp(stats::rnorm(n, log(500), 0.4)))
  situacion <- sample(c("Con Escritura", "Sin escritura"), n, TRUE, prob = c(0.7, 0.3))
  media <- 5 - 0.4 * log(superficie / 500) - 0.5 * (situacion == "Sin escritura")
  error <- stats::rnorm(n, 0, 0.2)
  y <- if (tipo == "lag") {
    solve(diag(n) - rho * W, media + error)
  } else {
    media + solve(diag(n) - rho * W, error)
  }
  sf::st_sf(id = seq_len(n), superficie = superficie, situacion_juridica = situacion,
            valor_m2 = exp(as.vector(y)),
            geom = sf::st_sfc(lapply(seq_len(n), function(i) sf::st_point(xy[i, ])), crs = 3857))
}

# Ejecuta calcular_vut en silencio y devuelve el resultado junto con los mensajes
correr_calcular_vut <- function(muestra, dist) {
  mensajes <- character(0)
  salida <- NULL
  withCallingHandlers(
    utils::capture.output(salida <- calcular_vut(
      df = muestra, index = "id", dependiente = "valor_m2",
      independientes = c("superficie" = "mediana", "situacion_juridica" = "Con Escritura"),
      dist = dist)),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  list(salida = salida, mensajes = mensajes)
}
