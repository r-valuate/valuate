# Calculo de variables de entorno

Esta funcion devuelve un raster que resume las caracteristicas del
vecindario a partir de capas geograficas vectoriales de poligonos o de
puntos.

## Usage

``` r
calcular_entorno(area, objeto, dim, ext, nombre)
```

## Arguments

- area:

  Poligono que indica el area sobre la cual se ejecutara el calculo.

- objeto:

  Objeto espacial "sf" con geometria del tipo "POLYGON" o "MULTIPOLYGON"
  sobre el que se calcularan las caracteristicas del entorno.

- dim:

  Tamano de cada pixel del raster resultante, en metros.

- ext:

  Extension del entorno o radio que define el vecindario, en metros
  lineales.

- nombre:

  Nombre de la variable resultante. El raster se guarda ademas en el
  directorio de trabajo con este nombre y la extension ".tif".

## Value

Un objeto 'SpatRaster' (libreria 'terra'). Cuando se le provee un objeto
espacial "sf" con geometria "POLYGON" o "MULTIPOLYGON" la funcion
permite calcular variables independientes que resumen las
caracteristicas del vecindario, indicando la proporcion del espacio que
se encuentra cubierto por los poligonos de interes. Por ejemplo, permite
calcular el porcentaje del vecindario que se encuentra cubierto por
plazas o parques. Cuando se le provee un objeto espacial "sf" con
geometria "POINT" la funcion permite calcular variables independientes
que resumen las caracteristicas del vecindario, indicando la cantidad de
puntos de interes que se encuentran dentro del entorno definido por el
usuario. Por ejemplo, permite calcular la cantidad de negocios u
oficinas que se encuentran dentro de un radio de "x" metros de cada
pixel.

## Author

Juan Pablo Carranza

## Examples

``` r
if (FALSE) { # \dontrun{
# Definimos el area de estudio
area_de_estudio <- nominatimlite::geo_lite_sf(address = "La Plata, Argentina", points_only = FALSE)
bbox = sf::st_transform(area_de_estudio, 4326) |> sf::st_bbox(bbox)

# Descargamos los parques que se encuentran dentro del area de estudio
plazas <- osmdata::opq(bbox) |>
  osmdata::add_osm_feature(key = "leisure", value = "park") |>
  osmdata::osmdata_sf()
plazas <- plazas$osm_polygons

# Calculamos un raster que resume, para cada pixel, las caracteristicas del entorno
plazas_entorno <- calcular_entorno(area = area_de_estudio,
                                   ext = 500,
                                   objeto = plazas,
                                   dim = 50,
                                   nombre = "plazas_entorno")
} # }
```
