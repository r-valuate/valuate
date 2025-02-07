#' Calculo de variables raster
#'
#' @description Esta funcion devuelve un raster que resume el calculo realizado a partir de otro raster, incluyendo fuentes de datos con diferente proyeccion y resolucion espacial.
#' @author Juan Pablo Carranza
#' @param raster  Raster original sobre el que se pretende realizar el calculo.
#' @param area Area sobre la que se desea realizar el calculo.
#' @param dim Tamano de cada pixel del raster resultante, en metros.
#' @param entorno Distancia sobre la que se pretende resumir la informacion del raster original. Default = 0. En todo otro caso se calcula el promedio de los valores de los pixeles en un area circular con un radio igual a la distancia establecida en este parametro.
#' @param nombre Nombre de la variable resultante. El raster sera guardado en el entorno de trabajo con este nombre y la extension ".tif".
#'
#' @return La funcion permite readecuar las resoluciones y proyecciones de diferentes rasters a las utilizadas en el proyecto. Ademas, el parametro "entorno" permite realizar un calculo del promedio de la variable bajo analisis en el vecindario definido por el usuario.
#' @export
#'
#' @examples
#' original = terra::rast(system.file("extdata/edificaciones.tif", package = "valuate"))
#' area_de_estudio <- sf::st_read(system.file("extdata/area.gpkg", package = "valuate"))
#' calcular_raster(original, area_de_estudio, dim = 50, entorno = 100, "prueba_raster")
calcular_raster <- function(raster, area, dim, entorno=0, nombre){
        raster = terra::project(raster, "epsg:3857")
        raster = terra::crop(raster, sf::st_transform(area, 3857))
                if(entorno > 0){
                fw <- raster::focalWeight(raster, entorno, "circle")
                raster <- terra::focal(x = raster, w = fw, fun = "mean")
                }
        names(raster) <- "entorno_edificaciones"
        r <- raster::raster(sf::st_transform(area, 3857), res = dim)
        r <- fasterize::fasterize(area, r)
        r <- terra::rast(r)
        terra::crs(r)  <- "epsg:3857"
        terra::values(r) <- 1
        a = raster::resample(raster::raster(raster), raster::raster(r), method='bilinear')
        terra::plot(a, col = viridis::plasma(100), main = paste0("Variable: ", nombre))
        terra::writeRaster(a, paste0(nombre, ".tif"), overwrite = TRUE)
        message(paste0("El proceso de calculo ha finalizado, y el raster resultante fue guardado en el directorio de trabajo con el nombre ", nombre, ".tif. En el environment se ha creado el raster '", nombre, "'."))
}
