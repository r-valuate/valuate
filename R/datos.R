#' Conjunto de datos de terrenos
#'
#' Este dataset contiene información sobre terrenos en venta, incluyendo el precio, moneda, tipo de valor, superficie y situación jurídica.
#'
#' @format Un data frame con las siguientes columnas:
#' \describe{
#'   \item{id}{Identificador único del terreno.}
#'   \item{valor}{Valor total del terreno en la moneda especificada.}
#'   \item{moneda}{Moneda en la que está expresado el valor del terreno (por ejemplo, "Dólar (USD)").}
#'   \item{tipo_valor}{Tipo de valor registrado (por ejemplo, "Oferta").}
#'   \item{superficie}{Superficie total del terreno en metros cuadrados.}
#'   \item{situacion_juridica}{Estado legal del terreno (por ejemplo, "Con Escritura", "Pre venta").}
#'   \item{valor_m2}{Valor por metro cuadrado del terreno.}
#'   \item{geom}{Coordenadas del terreno en el sistema de referencia utilizado.}
#' }
#' @source Datos generados a partir de anuncios de terrenos en venta.
"terrenos"

#' Conjunto de datos `datos`
#'
#' Este dataset contiene valores de utilidad territorial (`vut`) y su ubicación geográfica.
#'
#' @format Un data frame con las siguientes columnas:
#' \describe{
#'   \item{vut}{Valor de utilidad territorial del punto.}
#'   \item{geometry}{Coordenadas geográficas del punto.}
#' }
#' @source Datos geoespaciales procesados.
"dat"

#' Conjunto de datos `resultado`
#'
#' Este dataset contiene información detallada sobre valores inmobiliarios, características del entorno y distancias a distintos servicios.
#'
#' @format Un data frame con las siguientes columnas:
#' \describe{
#'   \item{id}{Identificador único del terreno.}
#'   \item{valor}{Valor total del terreno en la moneda especificada.}
#'   \item{moneda}{Moneda en la que está expresado el valor del terreno.}
#'   \item{tipo_valor}{Tipo de valor registrado.}
#'   \item{superficie}{Superficie total del terreno en metros cuadrados.}
#'   \item{situacion_juridica}{Estado legal del terreno.}
#'   \item{ID}{Identificador adicional para cruces de datos.}
#'   \item{area_plazas}{Porcentaje del entorno cubierto por plazas.}
#'   \item{comercios_entorno}{Cantidad de comercios en el entorno.}
#'   \item{d_bancos}{Distancia a bancos más cercanos.}
#'   \item{d_barrio_cerrado}{Distancia al barrio cerrado más cercano.}
#'   \item{d_escuela}{Distancia a la escuela más cercana.}
#'   \item{d_hospital}{Distancia al hospital más cercano.}
#'   \item{d_plazas}{Distancia a la plaza más cercana.}
#'   \item{d_policia}{Distancia a la comisaría más cercana.}
#'   \item{d_rio}{Distancia al río más cercano.}
#'   \item{d_universidad}{Distancia a la universidad más cercana.}
#'   \item{d_vias_primarias}{Distancia a la vía primaria más cercana.}
#'   \item{d_vias_secundarias}{Distancia a la vía secundaria más cercana.}
#'   \item{valor_m2}{Valor por metro cuadrado del terreno.}
#'   \item{coef}{Coeficiente ajustado.}
#'   \item{vut}{Valor de utilidad territorial ajustado.}
#'   \item{geometry}{Coordenadas geográficas del terreno.}
#' }
#' @source Datos derivados del procesamiento espacial y económico de terrenos.
"resultado"
