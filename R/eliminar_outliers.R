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
#' @return Un objeto 'sf' con las observaciones de la muestra original, sin las identificadas como espacialmente atipicas. La funcion permite identificar valores que son muy diferentes a los observados en el entorno. Para ello, compara el valor de cada observacion con el promedio del entorno (ponderado por la distancia, mediante la construccion de una matriz de pesos espaciales). Tambien permite identificar outliers espaciales mediante el calculo del indice de Moran local, siguiendo a Anselin (1995).
#' @export
#' @references
#' Anselin, L. (1995). Local Indicators of Spatial Association - LISA.
#' \emph{Geographical Analysis}, \bold{27}(2), 93-115.
#' \doi{10.1111/j.1538-4632.1995.tb00338.x}. Disponible en:
#' \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1538-4632.1995.tb00338.x}.
#' @examples
#'
#' # Cargamos la base de datos y tomamos las primeras 1500 observaciones
#' data("resultado", package = "valuate")
#' muestra <- resultado[1:1500, ]
#'
#' # Se define la estimacion de observaciones atipicas a partir de la variable 'vut'
#' # La distancia para el calculo es de 50 metros lineales. Esto define la extension del vecindario.
#' # El umbral para considerar a una observacion como atipica se define igual a 0.1 (+/- 10\%)
#' # No se realiza el calculo mediante el indice de Moran local.
#' depurados <- eliminar_outliers(df = muestra,
#'                                dist = 50,
#'                                variable = "vut",
#'                                umbral = 0.1,
#'                                moran = FALSE)
#'
#' # Si, en cambio, se desea identificar valores atipicos mediante
#' # el calculo del indice de Moran local:
#' depurados_moran <- eliminar_outliers(df = muestra,
#'                                      dist = 50,
#'                                      variable = "vut",
#'                                      moran = TRUE)
eliminar_outliers <- function(df, dist, variable, umbral, moran) {
  df = sf::st_as_sf(df)
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
          message(paste0("Se eliminaron ", nrow(subset(df, df$drop == "drop")), " observaciones espacialmente atipicas identificadas mediante el indice de Moran local."))
          df = subset(df, drop == "no drop")
          df$drop = NULL
          return(df)
  }
  else {
          w = spdep::listw2mat(lw)
          df$drop = ifelse(df[[variable]] / t(df[[variable]] %*% t(w)) == Inf, "no drop",
                           ifelse(df[[variable]] / t(df[[variable]] %*% t(w)) > (1 + umbral) |
                             df[[variable]] / t(df[[variable]] %*% t(w)) < (1 - umbral), "drop", "no drop"))
          table(df$drop)
          message(paste0("Se eliminaron ", nrow(subset(df, df$drop == "drop")), " observaciones espacialmente atipicas identificadas mediante el promedio ponderado del entorno."))
          df = subset(df, drop == "no drop")
          df$drop = NULL
          return(df)
}
}
