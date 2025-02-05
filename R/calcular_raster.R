#' Cálculo de variables ráster
#'
#' @description Esta función devuelve un ráster que resume el cálculo realizado a partir de otro ráster, incluyendo fuentes de datos con diferente proyección y resolución espacial.
#' @author Juan Pablo Carranza
#' @param raster  Ráster original sobre el que se pretende realizar el cálculo.
#' @param area Área sobre la que se desea realizar el cálculo.
#' @param dim Tamaño de cada píxel del ráster resultante, en metros.
#' @param entorno Distancia sobre la que se pretende resumir la información del ráster original. Default = 0. En todo otro caso se calcula el promedio de los valores de los píxeles en un área circular con un radio igual a la distancia establecida en este parámetro.
#' @param nombre Nombre de la variable resultante. El ráster será guardado en el entorno de trabajo con éste nombre y la extensión ".tif".
#'
#' @return La función permite readecuar las resoluciones y proyecciones de diferentes rásters a las utilizadas en el proyecto. Además, el paŕametro "entorno" permite realizar un cálculo del promedio de la variable bajo análisis en el vecindario definido por el usuario.
#' @export
#'
#' @examples
#' library(terra)
#' original = rast("edificaciones.tif")
#' library(sf)
#' area_de_estudio = st_read("area.gpkg")
#' calcular_raster(original, area_de_estudio, dim = 50, entorno = 100, "prueba_raster")
#'
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
        message(paste0("El proceso de cálculo ha finalizado, y el raster resultante se guardó en el directorio de trabajo con el nombre ", nombre, ".tif. En el environment se ha creado el ráster '", nombre, "'."))
}
