#' Homogeneizacion del precio de mercado de los inmuebles
#'
#' @description Esta funcion estima el valor de las observaciones muestrales expresado en terminos homogeneos o comparables.
#' @author Juan Pablo Carranza
#' @param df Objeto 'sf' con geometria 'POINT'. Muestra georreferenciada con precios de la tierra cuyo valor se pretende expresar en terminos homogeneos.
#' @param index Identificador unico para cada variable.
#' @param dependiente Nombre de la variable dependiente, aquella que contiene el precio por metro cuadrado.
#' @param independientes Vector con los nombres de las columnas a utilizarse como variables independientes, y el valor de referencia correspondiente a un "lote tipico". Si las variables son numericas se debe indicar si el ajuste a un lote tipico debe tomar como referencia la media o la mediana de dicha variable. En el caso de ser variables categoricas, se debe indicar el nombre de la categoria a tomarse como referencia. Ver ejemplo abajo.
#' @param dist Distancia que define el vecindario sobre el que se realizara la correccion por dependencia espacial.
#'
#' @return La funcion estima el precio por metro cuadrado homogeneizado, siguiendo a Carranza (2024). Toma como input un objeto espacial 'sf' y ajusta coeficientes de correccion segun diferentes variables definidas por el usuario para reexpresar el precio en termino de un 'lote tipico' para el area de estudio. La funcion devuelve un objeto llamado "resultado", que consiste en el df original al cual se agrega una columna correspondiente al valor homogeneizado (vut) y otra columna con el coeficiente de homogeneizacion aplicado (coef). Se informa, ademas, el parametro de ajuste estimado por el modelo lineal para cada una de las variables independientes involucradas en el proceso de homogeneizacion.
#' @export
#'
#' @examples
#' # Cargamos los datos correspondientes a una muestra de 1000 terrenos.
#' load("data/terrenos.rda")
#'
#' # Se define como "lote tipico" a aquel que se corresponde a una operacion de
#' # venta, realizada con escritura, y de una superficie igual a la mediana
#' # de los lotes de la ciudad.
#' # Se indica que la variable que informa sobre el precio por metro cuadrado
#' # es "valor_m2".
#' # Se define al vecindario en base a una distancia euclidiana de 500 metros.
#'
#' calcular_vut(df = datos,
#'              index = "id",
#'              independientes = c("tipo_valor" = "Venta",
#'                                 "superficie" = "mediana",
#'                                 "situacion_juridica" = "Con Escritura"),
#'              dependiente = "valor_m2",
#'              dist = 500)
calcular_vut <- function(df, index, dependiente, independientes, dist) {
  datos = df
  if ("media" %in% independientes) {val = "media"} else (val = "mediana")
  df <- subset(df, select = c(index[[1]], names(independientes), dependiente))
  df <- dplyr::rename(df, id = names(sf::st_drop_geometry(df[,index])))
  n <- nrow(df)
  df <- base::na.omit(df)
  if (nrow(df) < n) {message(paste0("Se eliminarán ", n - nrow(df), " observaciones con datos faltantes"))}
  n <- nrow(df)
  df <- sf::st_difference(df)
  if (nrow(df) < n) {message(paste0("Se eliminarán ", n - nrow(df), " observaciones con geometrías duplicadas"))}
  a <- sf::st_drop_geometry(df)
  a[sapply(a, is.character)] <- lapply(a[sapply(a, is.character)], as.factor)

  for (i in seq_along(independientes)) {
    n <- match(names(independientes)[[i]], names(a))
    if (is.factor(a[[n]])) {
      a[[n]] <- stats::relevel(a[[n]], ref = independientes[[i]])
    }
  }

  a[sapply(a, is.integer)] <- lapply(a[sapply(a, is.integer)], as.numeric)
  a <- dplyr::mutate_if(a, is.numeric, log)
  sf::st_geometry(df) <- "geom"
  df <- cbind(df[,c("id","geom")], a)

  form <- stats::reformulate(response = dependiente, names(independientes))
  ols <- lm(form, df)

  cord <- sf::st_coordinates(df)
  d <- suppressWarnings(spdep::dnearneigh(cord, 0, dist))
  dlist <- spdep::nbdists(d, sp::coordinates(cord))
  idlist <- lapply(dlist, function(x) 1/x)
  lw <- spdep::nb2listw(d, glist=idlist ,style="W" , zero.policy = TRUE)

  lm <- suppressMessages(spdep::lm.LMtests(ols, lw, test = c("RLMerr", "RLMlag", "SARMA"), zero.policy = TRUE))

  if (min(lm$SARMA$p.value, lm$RLMerr$p.value, lm$RLMlag$p.value) > 0.05) {
    print("No corresponde realizar correcciones por dependencia espacial, se procede a aplicar el modelo estimado por mínimos cuadrados ordinarios.")
  } else if (lm$SARMA$p.value <= 0.05) {
    message("Se procederá a realizar correcciones por dependencia espacial aplicando un modelo SARMA.")
    message("Este proceso puede demorar varios minutos, dependiendo de la cantidad de observaciones en la muestra.")
    sac <- spatialreg::sacsarlm(ols, data = df, listw = lw, zero.policy = T, na.action = na.omit)
  } else if (which.min(c(lm$RLMlag$p.value, lm$RLMerr$p.value)) == 1) {
    message("Se procederá a realizar correcciones por dependencia espacial aplicando un modelo de rezago espacial LAG.")
    sac <- spatialreg::lagsarlm(ols, data = df, listw = lw, zero.policy = T, na.action = na.omit)
  } else {
    message("Se procederá a realizar correcciones por dependencia espacial aplicando un modelo de rezago del ERROR espacial.")
    sac <- spatialreg::errorsarlm(ols, data = df, listw = lw, zero.policy = T, na.action = na.omit)
  }

  if (sac$type %in% c("sac", "lag")) {
    impactos <- spatialreg::impacts(sac, listw = lw)
    aux <- as.data.frame(summary(sac)$Coef) |>
      dplyr::mutate(Variable = rownames(as.data.frame(summary(sac)$Coef))) |>
      dplyr::select(.data$Variable, .data$`Pr(>|z|)`)
    row.names(aux) <- NULL
    aux <- dplyr::filter(aux, .data$Variable != "(Intercept)")
    impactos <- cbind(aux, impactos$total)
    impactos <- dplyr::filter(impactos, .data$`Pr(>|z|)` < 0.1)
    impactos <- dplyr::rename(impactos, Efecto = .data$total, `p-valor` = .data$`Pr(>|z|)`)
  } else {
    impactos <- as.data.frame(summary(sac)$Coef) |>
      dplyr::mutate(Variable = rownames(as.data.frame(summary(sac)$Coef))) |>
      dplyr::select(.data$Variable, .data$`Pr(>|z|)`, .data$Estimate)
    row.names(impactos) <- NULL
    impactos <- dplyr::filter(impactos, .data$Variable != "(Intercept)")
    impactos <- dplyr::filter(impactos, .data$`Pr(>|z|)` < 0.1)
    impactos <- dplyr::rename(impactos, Efecto = .data$Estimate, `p-valor` = .data$`Pr(>|z|)`)
  }

  message("Se procederá a calcular el valor unitario de referencia...")

  coef <- dummy_cols(df) |> sf::st_drop_geometry()
  coef <- suppressWarnings(data.table::melt(data.table::setDT(coef), id.vars = c("id"), variable.name = "Variable"))
  coef <- suppressMessages(dplyr::left_join(coef, impactos[,c("Variable","Efecto")]))
  coef$Valor <- suppressWarnings(as.numeric(coef$value))

  numericas <- dplyr::select(df, where(is.numeric)) |> names()
  factores <- dplyr::select(df, where(is.factor)) |> names()

  coef$tipo <- ifelse(coef$Variable %in% numericas, "Numerica", "No numerica")

  coef <- coef |>
    dplyr::group_by(.data$Variable) |>
    dplyr::mutate(
      mediana = if (val == "media") {mean(.data$Valor)} else {median(.data$Valor)},
      referencia = .data$Valor / .data$mediana
    )

  coef$impacto <- ifelse(
    coef$tipo == "Numerica", coef$referencia ^ coef$Efecto,
    ifelse(coef$tipo == "No numerica" & !is.na(coef$Efecto), coef$Valor * coef$Efecto, NA)
  )

  coef <- suppressMessages(dplyr::left_join(coef[, c("id", "impacto")], df[, c("id")], by = "id"))

  datos <- suppressMessages(dplyr::left_join(datos, coef, by = "id"))
  datos$vut <- sf::st_drop_geometry(datos[, dependiente]) * datos$impacto

  par(mfrow = c(1, 2))
  plot(as.vector(sf::st_drop_geometry(datos[,"vut"]))[[1]], as.vector(sf::st_drop_geometry(datos[,dependiente]))[[1]],
       xlab = "VUT", ylab = "Precio")
  abline(1,1)
  hist(datos$vut, xlab = "VUT", main = "")

  datos <- dplyr::rename_at(datos, dplyr::vars(id), ~index)
  assign("resultado", datos, envir=globalenv())

  message("El proceso de homogeneización ha finalizado. Se ha agregado al entorno de trabajo un nuevo objeto con la muestra original y una columna llamada 'vut'.")
  return(kableExtra::kable(impactos))
}

