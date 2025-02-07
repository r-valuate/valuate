##' Calculo de variables de entorno
#'
#' @description Esta funcion devuelve un raster que resume las caracteristicas del vecindario a partir de capas geograficas vectoriales de poligonos o de puntos.
#' @author Juan Pablo Carranza
#' @param area Poligono que indica el area sobre la cual se ejecutara el calculo.
#' @param objeto Objeto espacial "sf" con geometria del tipo "POLYGON" o "MULTIPOLYGON" sobre el que se calcularan las caracteristicas del entorno.
#' @param dim Tamano de cada pixel del raster resultante, en metros.
#' @param ext Extension del entorno o radio que define el vecindario, en metros lineales.
#' @param nombre Nombre de la variable resultante. El raster sera guardado en el entorno de trabajo con este nombre y la extension ".tif".
#'
#' @return Cuando se le provee un objeto espacial "sf" con geometria "POLYGON" o "MULTIPOLYGON" la funcion permite calcular variables independientes que resumen las caracteristicas del vecindario, indicando la proporcion del espacio que se encuentra cubierto por los poligonos de interes. Por ejemplo, permite calcular el porcentaje del vecindario que se encuentra cubierto por plazas o parques. Cuando se le provee un objeto espacial "sf" con geometria "POINT" la funcion permite calcular variables independientes que resumen las caracteristicas del vecindario, indicando la cantidad de puntos de interes que se encuentran dentro del entorno definido por el usuario. Por ejemplo, permite calcular la cantidad de negocios u oficinas que se encuentran dentro de un radio de "x" metros de cada pixel.
#' @export
#'
#' @examples
#' # Definimos el area de estudio
#' area_de_estudio <- nominatimlite::geo_lite_sf(address = "La Plata, Argentina", points_only = FALSE)
#' bbox = sf::st_transform(area_de_estudio, 4326) |> sf::st_bbox(bbox)
#'
#' # Descargamos los parques que se encuentran dentro del area de estudio
#' plazas <- osmdata::opq(bbox) |>
#'   osmdata::add_osm_feature(key = "leisure", value = "park") |>
#'   osmdata::osmdata_sf()
#' plazas <- plazas$osm_polygons
#'
#' # Calculamos un raster que resume, para cada pixel, las caracteristicas del entorno
#' calcular_entorno(area = area_de_estudio,
#'                  ext = 500,
#'                  objeto = plazas,
#'                  dim = 50,
#'                  nombre = "plazas_entorno")
calcular_entorno <- function(area, objeto, dim, ext, nombre){
  a = table(sf::st_geometry_type(objeto)) |> as.data.frame() |> dplyr::filter(Freq > 0)
  if(a$Var1 != "POINT" & a$Var1 != "POLYGON" & a$Var1 != "MULTIPOLYGON"){
    stop("Error: El objeto provisto para el cálculo debe tener geometrías de clase 'POINT', 'POLYGON' o 'MULTIPOLYGON'.")
  }
  if(a$Var1 == "POLYGON" | a$Var1 == "MULTIPOLYGON"){
  area <- sf::st_transform(area, 3857)
  area_aux <- area |> sf::st_buffer(ext*1.1)
  objeto <- sf::st_transform(objeto, 3857)
  objeto <- suppressWarnings(sf::st_intersection(objeto, area_aux))
  r <- raster::raster(area_aux, res = dim)
  r <- fasterize::fasterize(area_aux, r)
  r <- terra::crop(r, area_aux)
  r <- terra::rast(r)
  terra::crs(r)  <- "epsg:3857"
  terra::values(r) <- 1
  objeto$aux <- 1
  var <- fasterize::fasterize(objeto, raster::raster(r), field = "aux", fun = "sum")
  var <- terra::project(terra::rast(var), "epsg:3857")
  var <- terra::ifel(is.na(var), 0, var)
  fw <- raster::focalWeight(var, ext, "circle")
  var <- terra::focal(x = var, w = fw, fun = "mean")
  names(var) <- nombre
  var <- terra::crop(var, sf::st_transform(area, 3857))
  area <- sf::st_transform(area, 3857)
  r <- raster::raster(area, res = dim)
  r <- fasterize::fasterize(area, r)
  r <- terra::crop(r, area)
  r <- terra::rast(r)
  terra::crs(r)  <- "epsg:3857"
  var <- terra::resample(var, r, method='bilinear')
  var <- terra::mask(var, area)
  terra::plot(var, col = viridis::plasma(100), main = paste0("Variable: ", nombre))
  terra::writeRaster(var, paste0(nombre, ".tif"), overwrite = TRUE)
  assign(nombre, var, envir=globalenv())
  print(paste0("El proceso de cálculo ha finalizado, y el raster resultante se gardó en el directorio de trabajo con el nombre ", nombre, ".tif. En el environment se ha creado el ráster '", nombre, "'."))
  }
 else{
   area <- sf::st_transform(area, 3857)
   area_aux <- area |> sf::st_buffer(ext*1.1)
   objeto <- sf::st_transform(objeto, 3857)
   objeto <- suppressWarnings(sf::st_intersection(objeto, area_aux))
   r <- raster::raster(area_aux, res = dim)
   r <- fasterize::fasterize(area_aux, r)
   r <- terra::crop(r, area_aux)
   r <- terra::rast(r)
   terra::crs(r)  <- "epsg:3857"
   terra::values(r) <- 1
   objeto$aux <- 1
   var <- raster::rasterize(objeto, raster::raster(r), field = "aux", fun = "sum")
   var <- terra::project(terra::rast(var), "epsg:3857")
   var <- terra::ifel(is.na(var), 0, var)
   fw <- raster::focalWeight(var, ext, "circle")
   fw <- replace(fw, fw > 0, 1)
   var <- terra::focal(x = var, w = fw, fun = "sum")
   names(var) <- nombre
   var <- terra::crop(var, sf::st_transform(area, 3857))
   area <- sf::st_transform(area, 3857)
   r <- raster::raster(area, res = dim)
   r <- fasterize::fasterize(area, r)
   r <- terra::crop(r, area)
   r <- terra::rast(r)
   terra::crs(r)  <- "epsg:3857"
   var <- terra::resample(var, r, method='bilinear')
   var <- terra::mask(var, area)
   terra::plot(var, col = viridis::plasma(100), main = paste0("Variable: ", nombre))
   terra::writeRaster(var, paste0(nombre, ".tif"), overwrite = TRUE)
   assign(nombre, var, envir=globalenv())
   print(paste0("El proceso de cálculo ha finalizado, y el raster resultante se gardó en el directorio de trabajo con el nombre ", nombre, ".tif. En el environment se ha creado el ráster '", nombre, "'."))
 }
}
