#' Identificacion de valores espacialmente atipicos
#'
#' @description Esta funcion permite eliminar observaciones cuyo valor es muy diferente a los observados en el entorno. Permite identificar outliers espaciales mediante el calculo del indice de Moran local, o mediante el promedio observado en el entorno ponderado por la distancia.
#' @author Juan Pablo Carranza
#' @param df Dataframe sobre el que se realizara el calculo. Debe ser un objeto de la libreria 'sf' con geometria 'POINT'.
#' @param dist Distancia euclidiana que define la extension del vecindario en el que se compararan los valores.
#' @param variable Nombre de la variable sobre la que se realizara el calculo.
#' @param umbral Umbral para caracterizar a una observacion como atipica en relacion a lo observado en el vecindario. Por ejemplo, un umbral igual a 0.1 implicara que se caracterizaran como outliers a todas aquellas observaciones cuyo valor este por encima o por debajo del observado en el vecindario en una magnitud igual a +/- 10\%.
#' @param moran Logical. Si asume el valor 'TRUE' se procede a identificar outliers mediante el calculo del indice de Moran local. En este caso, el parametro 'umbral' queda sin efecto.
#'
#' @return La funcion permite identificar valores que son muy diferentes a los observados en el entorno. Para ello, compara el valor de cada observacion con el promedio del entorno (ponderado por la distancia, mediante la construccion de una matriz de pesos espaciales). Tambien permite identificar outliers espaciales mediante el calculo del indice de Moran local, siguiendo a Anselin (1995).
#' @export
#' @references
#' Anselin, L. (1995). Local Indicators of Spatial Association—LISA.
#' \emph{Geographical Analysis}, \bold{27}(2), 93–115.
#' \doi{10.1111/j.1538-4632.1995.tb00338.x}. Disponible en:
#' \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1538-4632.1995.tb00338.x}.
#' @examples
#'
#' # Cargamos la base de datos
#' load("resultado.rda")
#'
#' # Se define la estimacion de observaciones atipicas a partir de la variable 'vut'
#' # La distancia para el calculo es de 50 metros lineales. Esto define la extension del vecindario.
#' # El umbral para considerar a una observacion como atipica se define igual a 0.1 (+/- 10\%)
#' # No se realiza el calculo mediante el indice de Moran local.
#' eliminar_outliers(df = resultado,
#'                   dist = 50,
#'                   variable = "vut",
#'                   umbral = 0.1,
#'                   moran = FALSE)
#'
#' # Si, en cambio, se desea identificar valores atipicos mediante el calculo del indice de Moran local:
#' eliminar_outliers(df = resultado,
#'                   dist = 50,
#'                   variable = "vut",
#'                   moran = TRUE)
eliminar_outliers <- function(df, dist, variable, umbral, moran) {
  df = subset(df, is.na(df[[variable]]) == FALSE)
  cord <- sf::st_coordinates(df)
  d <- suppressWarnings(spdep::dnearneigh(cord, 0, dist))
  dlist <- spdep::nbdists(d, sp::coordinates(cord))
  idlist <- lapply(dlist, function(x) 1/x)
  lw <- spdep::nb2listw(d, glist=idlist ,style="W" , zero.policy = TRUE)
  if (moran == TRUE){
          moran = as.data.frame(spdep::localmoran(df[[variable]], lw, zero.policy = TRUE))
          moran$drop = ifelse(moran$Ii < 0 & moran$`Pr(z != E(Ii))` < 0.05, "drop", "no drop")
          df$drop = moran$drop
          print(paste0("Se eliminaron ", nrow(subset(df, df$drop == "drop")), " observaciones espacialmente atípicas calculadas mediante el índice de Moran local. Se creó un objeto en el environment llamado 'depurados' que contiene la base de datos original sin los outliers."))
          df = subset(df, drop == "no drop")
          df$drop = NULL
  }
  else {
          w = spdep::listw2mat(lw)
          df$drop = ifelse(df[[variable]] / t(df[[variable]] %*% t(w)) == Inf, "no drop",
                           ifelse(df[[variable]] / t(df[[variable]] %*% t(w)) > (1 + umbral) |
                             df[[variable]] / t(df[[variable]] %*% t(w)) < (1 - umbral), "drop", "no drop"))
          table(df$drop)
          print(paste0("Se eliminaron ", nrow(subset(df, df$drop == "drop")), " observaciones espacialmente atípicas calculadas mediante el índice de Moran local. Se creó un objeto en el environment llamado 'depurados' que contiene la base de datos original sin los outliers."))
          df = subset(df, drop == "no drop")
          df$drop = NULL
          assign("depurados", df, envir=globalenv())
}
}
