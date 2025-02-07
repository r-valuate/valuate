#' Simulacion de escenarios y estimacion de externalidades espaciales
#' @description A partir de la prediccion de la distribucion espacial de una variable, la funcion permite estimar escenarios a partir de la simulacion de alteraciones en alguna (o algunas) de las variables independientes.
#'
#' @author Juan Pablo Carranza
#'
#' @param modelo Archivo .rda guardado en el directorio de trabajo por la funcion estimar_modelo().
#' @param independientes Vector con los nombres de las variables independientes. Debe tener los mismos nombre que los utilizados en la funcion estimar_modelo(), solo que admite que los rasters tengan una distribucion espacial diferente, incorporando modificaciones simuladas a la nueva prediccion.
#' @param original Resultado del modelo original. Es un archivo .tif guardado en el directorio de trabajo por la funcion estimar_modelo().
#'
#' @return La funcion devuelve un raster que informa sobre el impacto en la variable dependiente de las simulaciones realizadas en las variables independientes. Permite estimar la magnitud y el alcance territorial de las modificaciones simuladas sobre la variable de estudio.
#' @export
simular_escenario <- function(modelo, independientes, original){
  load(modelo)
  pred = raster::stack(independientes)
  simulacion = terra::interpolate(terra::rast(pred), qrf, na.rm = TRUE, wopt=list(steps=80))
  terra::writeRaster(simulacion, "simulacion.tif", overwrite = TRUE)
  original <- terra::rast("~/Documentos/Consultorias/BID/test/entrenar_modelo/vut.tif")
  names(original) <- "original"
  names(simulacion) <- "simulacion"
  original = round(original, 0)
  simulacion = round(simulacion, 0)
  ver = original - simulacion
  a = raster::rasterToPoints(raster::raster(ver), spatial = TRUE)
  # a = subset(a, a$original != 0)
  a = subset(a, a$original > 0)
  resultado <- terra::interpIDW(terra::rast(pred), terra::vect(sf::st_as_sf(a)), field = "original",
                                radius=200, power=3, smooth=10, maxPoints=20, minPoints = 5,
                                fill = NA)
  resultado <- raster::trim(resultado)
  terra::plot(resultado, smooth = TRUE)
  terra::writeRaster(resultado, "impacto.tif", overwrite=TRUE)
  message("En el directorio de trabajo se ha guardado un raster con el impacto de la simulacion, con el nombre 'impacto.tif'.")
}
