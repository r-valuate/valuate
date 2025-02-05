#' Calculo de variables de distancia
#'
#' @description Esta funcion devuelve un raster con las distancias euclidianas, en metros, desde cada pixel hacia el objeto vectorial mas cercano.
#' @author Juan Pablo Carranza
#' @param area Poligono que indica el area sobre la cual se ejecutara el calculo.
#' @param objeto Objeto espacial "sf" contra el que se calcularan las distancias.
#' @param dim Tamano de cada pixel del raster resultante, en metros.
#' @param nombre Nombre de la variable resultante. El raster sera guardado en el entorno de trabajo con este nombre y la extension ".tif".
#'
#' @return Se genera un raster que informa, para cada pixel, sobre la distancia (en metros lineales) al objeto mas cercano.
#' @export
#'
#' @examples
#' # Definimos el area de estudio
#' area_de_estudio <- nominatimlite::geo_lite_sf(address = "La Plata, Argentina", points_only = FALSE)
#' bbox = sf::st_transform(area_de_estudio, 4326) |> st_bbox(bbox)
#'
#' # Descargamos las vias principales
#' vias_prim <- osmdata::opq(bbox) |>
#'   osmdata::add_osm_feature(key = "highway", value = "secondary") |>
#'   osmdata::osmdata_sf()
#' vias_prim <- vias_prim$osm_lines
#'
#' # Calculamos un raster con las distancias a la via principal mas cercana
#' calcular_dist(area = area_de_estudio,
#'               objeto = vias_prim,
#'               dim = 50,
#'               nombre = "vias")
calcular_dist <- function(area, objeto, dim, nombre){
  area <- sf::st_transform(area, 3857)
  objeto <- suppressWarnings(sf::st_transform(objeto, 3857))
  r <- raster::raster(area, res = dim)
  r <- fasterize::fasterize(area, r)
  r <- terra::crop(r, area)
  r <- terra::rast(r)
  terra::crs(r)  <- "epsg:3857"
  var <- terra::distance(r, terra::vect(as(objeto,"Spatial")))
  var <- terra::crop(var, terra::vect(as(area, "Spatial")), mask=TRUE)
  names(var) <- nombre
  terra::plot(var, col = viridis::plasma(100), main = paste0("Variable: ", nombre))
  terra::writeRaster(var, paste0(nombre, ".tif"), overwrite = TRUE)
  assign(nombre, var, envir=globalenv())
  message(paste0("El proceso de cálculo ha finalizado, y el raster resultante se gardó en el directorio de trabajo con el nombre ", nombre, ".tif. En el environment se ha creado el ráster '", nombre, "'."))
}
