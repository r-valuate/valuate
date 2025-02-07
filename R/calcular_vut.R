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
#' data("terrenos", package = "valuate")
#'
#' # Se define como "lote tipico" a aquel que se corresponde a una operacion de
#' # venta, realizada con escritura, y de una superficie igual a la mediana
#' # de los lotes de la ciudad.
#' # Se indica que la variable que informa sobre el precio por metro cuadrado
#' # es "valor_m2".
#' # Se define al vecindario en base a una distancia euclidiana de 500 metros.
#'
#' calcular_vut(df = terrenos,
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
  df <- stats::na.omit(df)
  if (nrow(df) < n) {message(paste0("Se eliminarán ", n - nrow(df), " observaciones con datos faltantes"))}
  n <- nrow(df)
  df <- sf::st_difference(df)
  if (nrow(df) < n) {message(paste0("Se eliminarán ", n - nrow(df), " observaciones con geometrías duplicadas"))}
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
  message("Se procederá a calcular el valor unitario de referencia...")
  dummy_cols <- function (.data, select_columns = NULL, remove_first_dummy = FALSE,
                          remove_most_frequent_dummy = FALSE, ignore_na = FALSE, split = NULL,
                          remove_selected_columns = FALSE, omit_colname_prefix = FALSE)
  {
    stopifnot(is.null(select_columns) || is.character(select_columns),
              select_columns != "", is.logical(remove_first_dummy),
              length(remove_first_dummy) == 1, is.logical(remove_selected_columns))
    if (remove_first_dummy == TRUE & remove_most_frequent_dummy ==
        TRUE) {
      stop("Select either 'remove_first_dummy' or 'remove_most_frequent_dummy'\n         to proceed.")
    }
    if (is.vector(.data)) {
      .data <- data.frame(.data = .data, stringsAsFactors = FALSE)
    }
    if (!is.null(select_columns)) {
      char_cols <- select_columns
      cols_not_in_data <- char_cols[!char_cols %in% names(.data)]
      char_cols <- char_cols[!char_cols %in% cols_not_in_data]
      if (length(char_cols) == 0) {
        stop("select_columns is/are not in data. Please check data and spelling.")
      }
    }
    else if (ncol(.data) == 1) {
      char_cols <- names(.data)
    }
    else {
      char_cols <- sapply(.data, class)
      char_cols <- char_cols[char_cols %in% c("factor", "character")]
      char_cols <- names(char_cols)
    }
    if (length(char_cols) == 0 && is.null(select_columns)) {
      stop(paste0("No character or factor columns found. ",
                  "Please use select_columns to choose columns."))
    }
    if (!is.null(select_columns) && length(cols_not_in_data) >
        0) {
      warning(paste0("NOTE: The following select_columns input(s) ",
                     "is not a column in data.\n"), paste0(names(cols_not_in_data),
                                                           "\t"))
    }
    for (col_name in char_cols) {
      if (is.factor(.data[[col_name]])) {
        unique_vals <- levels(.data[[col_name]])
        if (any(is.na(.data[[col_name]]))) {
          unique_vals <- c(unique_vals, NA)
        }
      }
      else {
        unique_vals <- unique(.data[[col_name]])
        unique_vals <- stringr::str_sort(unique_vals, na_last = TRUE,
                                         locale = "en_US", numeric = TRUE)
      }
      unique_vals <- as.character(unique_vals)
      if (!is.null(split)) {
        unique_vals <- unique(trimws(unlist(strsplit(unique_vals,
                                                     split = split))))
      }
      if (ignore_na) {
        unique_vals <- unique_vals[!is.na(unique_vals)]
      }
      if (remove_most_frequent_dummy) {
        vals <- as.character(.data[[col_name]])
        vals <- data.frame(sort(table(vals), decreasing = TRUE),
                           stringsAsFactors = FALSE)
        top_vals <- vals[vals$Freq %in% max(vals$Freq),
        ]
        other_vals <- vals$vals[!vals$Freq %in% max(vals$Freq)]
        other_vals <- as.character(other_vals)
        top_vals <- top_vals[stringr::str_order(top_vals$vals,
                                                na_last = TRUE, locale = "en_US", numeric = TRUE),
        ]
        if (nrow(top_vals) == 1) {
          top_vals <- NULL
        }
        else {
          top_vals <- as.character(top_vals$vals[2:nrow(top_vals)])
        }
        unique_vals <- c(top_vals, other_vals)
        unique_vals <- stringr::str_sort(unique_vals, na_last = TRUE,
                                         locale = "en_US", numeric = TRUE)
      }
      if (remove_first_dummy) {
        unique_vals <- unique_vals[-1]
      }
      data.table::alloc.col(.data, ncol(.data) + length(unique_vals))
      .data[, paste0(col_name, "", unique_vals)] <- 0L
      for (unique_value in unique_vals) {
        data.table::set(.data, i = which(data.table::chmatch(as.character(.data[[col_name]]),
                                                             unique_value, nomatch = 0) == 1L), j = paste0(col_name,
                                                                                                           "", unique_value), value = 1L)
        if (!is.na(unique_value)) {
          data.table::set(.data, i = which(is.na(.data[[col_name]])),
                          j = paste0(col_name, "", unique_value), value = NA)
        }
        if (!is.null(split)) {
          max_split_length <- max(sapply(strsplit(as.character(.data[[col_name]]),
                                                  split = split), length))
          for (split_length in 1:max_split_length) {
            data.table::set(.data, i = which(data.table::chmatch(as.character(trimws(sapply(strsplit(as.character(.data[[col_name]]),
                                                                                                     split = split), `[`, split_length))), unique_value,
                                                                 nomatch = 0) == 1L), j = paste0(col_name,
                                                                                                 "", unique_value), value = 1L)
          }
          if (is.na(unique_value)) {
            .data[[paste0(col_name, "", unique_value)]][which(!is.na(.data[[col_name]]))] <- 0
          }
        }
      }
    }
    if (omit_colname_prefix) {
      if (length(select_columns) == 1) {
        new_col_index <- as.logical(rowSums(sapply(unique_vals,
                                                   function(x) grepl(paste0(select_columns, "",
                                                                            x), names(.data)))))
        names(.data)[new_col_index] <- gsub(paste0(select_columns,
                                                   ""), "", names(.data)[new_col_index])
      }
      else {
        message("Can't omit the colname prefix when recoding more than one column.")
        message("Returning prefixed dummy columns.")
      }
    }
    return(.data)
  }
  coef <- dummy_cols(df) |> sf::st_drop_geometry()
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
  assign("resultado", datos, envir=globalenv())
  message("El proceso de homogeneización ha finalizado. Se ha¨ agregado al entorno de trabajo un nuevo objeto que contiene la muestra original, más una nueva columna llamada 'vut', que contiene el precio reexpresado en términos homogéneos.")
  message("Los efectos estadísticamene significativos fueron los siguientes:")
  return(kableExtra::kable(impactos))
}

