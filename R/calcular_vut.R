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
#' @return Una lista con dos elementos: \code{datos}, la muestra original (objeto 'sf') con dos columnas nuevas, \code{vut} (precio homogeneizado) y \code{coef} (coeficiente de homogeneizacion aplicado); y \code{efectos}, una tabla con los efectos estadisticamente significativos de cada variable. La lista se devuelve de forma invisible, y la tabla de efectos se imprime en la consola. La funcion estima el precio por metro cuadrado homogeneizado, siguiendo a Carranza (2024). Toma como input un objeto espacial 'sf' y ajusta coeficientes de correccion segun diferentes variables definidas por el usuario para reexpresar el precio en termino de un 'lote tipico' para el area de estudio. Se informa, ademas, el parametro de ajuste estimado por el modelo lineal para cada una de las variables independientes involucradas en el proceso de homogeneizacion.
#' @export
#'
#' @examples
#' \dontrun{
#' # Cargamos los datos correspondientes a una muestra de 1000 terrenos.
#' data("terrenos", package = "valuate")
#'
#' # Se define como "lote tipico" a aquel que se corresponde a una operacion de
#' # venta, realizada con escritura, y de una superficie igual a la mediana
#' # de los lotes de la ciudad.
#' # Se indica que la variable que informa sobre el precio por metro cuadrado
#' # es "valor_m2".
#' # Se define al vecindario en base a una distancia euclidiana de 500 metros.
#'
#' vut <- calcular_vut(df = terrenos,
#'                     index = "id",
#'                     independientes = c("tipo_valor" = "Venta",
#'                                        "superficie" = "mediana",
#'                                        "situacion_juridica" = "Con Escritura"),
#'                     dependiente = "valor_m2",
#'                     dist = 500)
#'
#' # Muestra con el precio homogeneizado
#' vut$datos
#'
#' # Efectos estimados
#' vut$efectos
#' }
calcular_vut <- function(df, index, dependiente, independientes, dist) {
  datos = df
  if ("media" %in% independientes) {val = "media"} else (val = "mediana")
  df <- subset(df, select = c(index[[1]], names(independientes), dependiente))
  df <- dplyr::rename(df, id = names(sf::st_drop_geometry(df[,index])))
  n <- nrow(df)
  df <- stats::na.omit(df)
  if (nrow(df) < n) {message(paste0("Se eliminaran ", n - nrow(df), " observaciones con datos faltantes"))}
  n <- nrow(df)
  df <- sf::st_difference(df)
  if (nrow(df) < n) {message(paste0("Se eliminaran ", n - nrow(df), " observaciones con geometrias duplicadas"))}
  a <- sf::st_drop_geometry(df)
  a[sapply(a, is.character)] <- lapply(a[sapply(a, is.character)],
                                       as.factor)
  for (i in c(1:length(independientes))) {
    n <- match(names(independientes)[[i]], names(a))
    if (is.factor(a[,n] )){
      a[,n] <- stats::relevel(a[,n], ref = independientes[[i]])
    }
  }
  a[sapply(a, is.integer)] <- lapply(a[sapply(a, is.integer)],
                                     as.numeric)
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
  # Desde spdep 1.3-3 los tests robustos se llaman "adjRSerr" y "adjRSlag"
  p_sarma <- lm$SARMA$p.value
  # (antes "RLMerr" y "RLMlag"); se toma el que exista
  p_err <- c(lm$adjRSerr$p.value, lm$RLMerr$p.value)[1]
  p_lag <- c(lm$adjRSlag$p.value, lm$RLMlag$p.value)[1]
  if (min(p_sarma, p_err, p_lag) > 0.05) {
    print("No corresponde realizar correcciones por dependencia espacial, se procede a aplicar el modelo estimado por minimos cuadrados ordinarios.")
    sac <- NULL
  } else if (p_sarma <= 0.05) {
    message("Se procedera a realizar correcciones por dependencia espacial aplicando un modelo SARMA.")
    message("Este proceso puede demorar varios minutos, dependiendo de la cantidad de observaciones en la muestra.")
    sac <- spatialreg::sacsarlm(ols, data = df, listw = lw, zero.policy = T, na.action = na.omit)
  } else if (which.min(c(p_lag, p_err)) == 1) {
    message("Se procedera a realizar correcciones por dependencia espacial aplicando un modelo de rezago espacial LAG.")
    sac <- spatialreg::lagsarlm(ols, data = df, listw = lw, zero.policy = T, na.action = na.omit)
  } else {
    message("Se procedera a realizar correcciones por dependencia espacial aplicando un modelo de rezago del ERROR espacial.")
    sac <- spatialreg::errorsarlm(ols, data = df, listw = lw, zero.policy = T, na.action = na.omit)
  }
  if (is.null(sac)) {
    coeficientes <- summary(ols)$coefficients
    impactos <- data.frame(Variable = rownames(coeficientes),
                           `p-valor` = coeficientes[, "Pr(>|t|)"],
                           Efecto = coeficientes[, "Estimate"],
                           check.names = FALSE)
    impactos <- impactos[impactos$Variable != "(Intercept)" & impactos$`p-valor` < 0.1, ]
    row.names(impactos) <- NULL
  } else if (sac$type %in% c("sac", "lag")) {
    impactos <- spatialreg::impacts(sac, listw = lw)
    aux <- as.data.frame(summary(sac)$Coef) |>
      dplyr::mutate(Variable = rownames(as.data.frame(summary(sac)$Coef))) |>
      dplyr::select(Variable, `Pr(>|z|)`)
    row.names(aux) <- NULL
    aux <- dplyr::filter(aux, Variable != "(Intercept)")
    impactos <- cbind(aux, impactos$total)
    impactos <- dplyr::filter(impactos, `Pr(>|z|)` < 0.1)
    impactos <- dplyr::rename(impactos, Efecto = `impactos$total`,
                              `p-valor` = `Pr(>|z|)`)
  } else {
    impactos <- as.data.frame(summary(sac)$Coef) |>
      dplyr::mutate(Variable = rownames(as.data.frame(summary(sac)$Coef))) |>
      dplyr::select(Variable, `Pr(>|z|)`, Estimate)
    row.names(impactos) <- NULL
    impactos <- dplyr::filter(impactos, Variable != "(Intercept)")
    impactos <- dplyr::filter(impactos, `Pr(>|z|)` < 0.1)
    impactos <- dplyr::rename(impactos, Efecto = Estimate,
                              `p-valor` = `Pr(>|z|)`)
  }
  message("Se procedera a calcular el valor unitario de referencia...")
  coef <- crear_dummies(df) |> sf::st_drop_geometry()
  coef <- suppressWarnings(data.table::melt(data.table::setDT(coef), id.vars = c("id"), variable.name = "Variable"))
  coef <- suppressMessages(dplyr::left_join(coef, impactos[,c("Variable","Efecto")]))
  coef$Valor <- suppressWarnings(as.numeric(coef$value))
  aux <- sf::st_drop_geometry(df)
  numericas <- aux |> dplyr::select(dplyr::where(is.numeric)) |> names()
  numericas <- numericas[numericas %in% names(independientes)]
  factores <- aux |> dplyr::select(dplyr::where(is.factor)) |> names()
  factores <- factores[factores %in% names(independientes)]
  coef$tipo <- ifelse(coef$Variable %in% numericas, "Numerica", "No numerica")
  coef <- coef |>
    dplyr::group_by(Variable) |>
    dplyr::mutate(mediana = if (val == "media") {mean(Valor)} else (stats::median(Valor)),
                  referencia = Valor / mediana)
  coef$impacto <- ifelse(coef$tipo == "Numerica", coef$referencia ^ coef$Efecto,
                         ifelse(coef$tipo == "No numerica" & is.na(coef$Efecto) == F,
                                coef$Valor * coef$Efecto, NA))



  primero <- coef |>
    dplyr::group_by(Variable, id) |>
    dplyr::filter(tipo == "Numerica") |>
    dplyr::mutate(aux = prod(impacto)) |>
    dplyr::arrange(id) |>
    stats::na.omit()

  segundo <- coef |>
    dplyr::group_by(id) |>
    dplyr::filter(tipo == "No numerica") |>
    dplyr::arrange(id) |>
    dplyr::summarise(exp = exp(sum(impacto, na.rm = T)))
  coef <- suppressMessages(dplyr::left_join(primero[,c("id","aux")], segundo))
  coef$coef <- coef$aux * coef$exp
  datos <- suppressMessages(dplyr::left_join(datos, coef[,c("id","coef")]))
  datos[,"vut"] <- sf::st_drop_geometry(datos[,dependiente]) * datos$coef
  graphics::par(mfrow = c(1, 2))
  datos <- datos |> dplyr::select(dplyr::everything(), attr(datos,"sf_column"))
  plot(as.vector(sf::st_drop_geometry(datos[,"vut"]))[[1]], as.vector(sf::st_drop_geometry(datos[,dependiente]))[[1]],
       xlab = "VUT", ylab = "Precio")
  graphics::abline(1,1)
  graphics::hist(datos$vut, xlab = "VUT", main = "")
  datos <- dplyr::rename_at(datos, dplyr::vars(id), ~index)
  message("El proceso de homogeneizacion ha finalizado.")
  message("Los efectos estadisticamente significativos fueron los siguientes:")
  print(kableExtra::kable(impactos))
  return(invisible(list(datos = datos, efectos = impactos)))
}


# Agrega una columna indicadora (0/1) por cada categoria de las variables
# categoricas (factor o character) de 'datos'. Las observaciones con valor
# faltante quedan como NA en las columnas indicadoras; si la variable tiene
# faltantes se agrega ademas una columna "<variable>NA".
# Reemplaza a fastDummies::dummy_cols() con sus opciones por defecto.
crear_dummies <- function(datos) {
  clases <- lapply(datos, class)
  categoricas <- names(datos)[vapply(clases, function(k) {
    length(k) == 1 && k %in% c("factor", "character")
  }, logical(1))]
  for (variable in categoricas) {
    x <- datos[[variable]]
    if (is.factor(x)) {
      categorias <- levels(x)
      if (anyNA(x)) categorias <- c(categorias, NA)
    } else {
      categorias <- stringr::str_sort(unique(x), na_last = TRUE, locale = "en_US", numeric = TRUE)
    }
    for (categoria in as.character(categorias)) {
      nombre <- paste0(variable, categoria)
      if (is.na(categoria)) {
        datos[[nombre]] <- as.integer(is.na(x))
      } else {
        datos[[nombre]] <- ifelse(is.na(x), NA_integer_, as.integer(as.character(x) == categoria))
      }
    }
  }
  datos
}
