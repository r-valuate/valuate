# Calculo de variables raster

Esta funcion devuelve un raster que resume el calculo realizado a partir
de otro raster, incluyendo fuentes de datos con diferente proyeccion y
resolucion espacial.

## Usage

``` r
calcular_raster(raster, area, dim, entorno = 0, nombre)
```

## Arguments

- raster:

  Raster original sobre el que se pretende realizar el calculo.

- area:

  Area sobre la que se desea realizar el calculo.

- dim:

  Tamano de cada pixel del raster resultante, en metros.

- entorno:

  Distancia sobre la que se pretende resumir la informacion del raster
  original. Default = 0. En todo otro caso se calcula el promedio de los
  valores de los pixeles en un area circular con un radio igual a la
  distancia establecida en este parametro.

- nombre:

  Nombre de la variable resultante. El raster se guarda ademas en el
  directorio de trabajo con este nombre y la extension ".tif".

## Value

Un objeto 'SpatRaster' (libreria 'terra'). La funcion permite readecuar
las resoluciones y proyecciones de diferentes rasters a las utilizadas
en el proyecto. Ademas, el parametro "entorno" permite realizar un
calculo del promedio de la variable bajo analisis en el vecindario
definido por el usuario.

## Author

Juan Pablo Carranza

## Examples

``` r
if (FALSE) { # \dontrun{
original <- terra::rast(descargar_datos("edificaciones.tif"))
area_de_estudio <- sf::st_read(system.file("extdata/area.gpkg", package = "valuate"))
edificaciones <- calcular_raster(original, area_de_estudio, dim = 50, entorno = 100,
                                 nombre = "edificaciones")
} # }
```
