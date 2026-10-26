# Descarga de los datos de ejemplo

Descarga los rasters de ejemplo de la Ciudad de Medellin (Colombia) que
se utilizan en los ejemplos de la documentacion y en el README. Por su
tamano, estos archivos no se incluyen en el paquete: se descargan desde
el repositorio del proyecto en GitHub y se guardan en una carpeta de
cache del usuario, por lo que solo se descargan la primera vez.

## Usage

``` r
descargar_datos(
  archivos = datos_disponibles(),
  directorio = tools::R_user_dir("valuate", "cache"),
  sobrescribir = FALSE
)
```

## Arguments

- archivos:

  Nombres de los archivos a descargar. Por defecto, todos los
  disponibles (ver
  [`datos_disponibles()`](https://r-valuate.github.io/valuate/reference/datos_disponibles.md)).

- directorio:

  Carpeta en la que se guardan los archivos. Por defecto, la carpeta de
  cache del paquete, que se obtiene con
  `tools::R_user_dir("valuate", "cache")`.

- sobrescribir:

  Logical. Si es 'TRUE', los archivos se descargan nuevamente aunque ya
  existan.

## Value

Un vector con las rutas a los archivos descargados, que puede pasarse
directamente a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
o al argumento `independientes` de
[`entrenar_modelo()`](https://r-valuate.github.io/valuate/reference/entrenar_modelo.md).

## Author

Juan Pablo Carranza

## Examples

``` r
datos_disponibles()
#>  [1] "dist_basura.tif"            "dist_industria.tif"        
#>  [3] "dist_plazas_parques.tif"    "dist_tren.tif"             
#>  [5] "dist_vias_autopista.tif"    "dist_vias_prim.tif"        
#>  [7] "dist_vias_sec.tif"          "edificaciones.tif"         
#>  [9] "entorno_altura.tif"         "entorno_comercios.tif"     
#> [11] "entorno_edificaciones.tif"  "entorno_hoteles.tif"       
#> [13] "entorno_plazas_parques.tif"
if (FALSE) { # \dontrun{
rutas <- descargar_datos(c("dist_tren.tif", "entorno_comercios.tif"))
terra::rast(rutas)
} # }
```
