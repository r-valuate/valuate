#' Cálculo de variables de distancia
#'
#' @description Esta función devuelve un ráster con las distancias euclidianas, en metros, desde cada píxel hacia el objeto vectorial más cercano.
#' @author Juan Pablo Carranza
#' @param area Polígono que indica el área sobre la cuál se ejecutará el cálculo.
#' @param objeto Objeto espacial "sf" contra el que se calcularán las distancias.
#' @param dim Tamaño de cada píxel del ráster resultante, en metros.
#' @param nombre Nombre de la variable resultante. El ráster será guardado en el entorno de trabajo con éste nombre y la extensión ".tif".
#'
#' @return Se genera un ráster que informa, para cada píxel, sobre la distancia (en metros lineales) al objeto más cercano.
#' @export
#'
#' @examples
#' # Definimos el área de estudio
#' library(nominatimlite)
#' library(osmdata)
#' library(sf)
#' area_de_estudio <- geo_lite_sf(address = "La Plata, Argentina", points_only = F)
#' bbox = st_transform(area_de_estudio, 4326) |> st_bbox(bbox)
#'
#' # Descargamos las vias principales
#' vias_prim <- opq(bbox) |>
#'   add_osm_feature(key = "highway", value = "secondary") |>
#'   osmdata_sf()
#' vias_prim <- vias_prim$osm_lines
#'
#' # Calculamos un ráster con las distancias a la vía principal más cercana
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
