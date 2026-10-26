# Calculo de variables de distancia

Esta funcion devuelve un raster con las distancias euclidianas, en
metros, desde cada pixel hacia el objeto vectorial mas cercano.

## Usage

``` r
calcular_dist(area, objeto, dim, nombre)
```

## Arguments

- area:

  Poligono que indica el area sobre la cual se ejecutara el calculo.

- objeto:

  Objeto espacial "sf" contra el que se calcularan las distancias.

- dim:

  Tamano de cada pixel del raster resultante, en metros.

- nombre:

  Nombre de la variable resultante. El raster se guarda ademas en el
  directorio de trabajo con este nombre y la extension ".tif".

## Value

Un objeto 'SpatRaster' (libreria 'terra'). Se genera un raster que
informa, para cada pixel, sobre la distancia (en metros lineales) al
objeto mas cercano.

## Author

Juan Pablo Carranza

## Examples

``` r
if (FALSE) { # \dontrun{
# Definimos el area de estudio
area_de_estudio <- nominatimlite::geo_lite_sf(address = "La Plata, Argentina", points_only = FALSE)
bbox = sf::st_transform(area_de_estudio, 4326) |> sf::st_bbox(bbox)

# Descargamos las vias principales
vias_prim <- osmdata::opq(bbox) |>
  osmdata::add_osm_feature(key = "highway", value = "secondary") |>
  osmdata::osmdata_sf()
vias_prim <- vias_prim$osm_lines

# Calculamos un raster con las distancias a la via principal mas cercana
vias <- calcular_dist(area = area_de_estudio,
                      objeto = vias_prim,
                      dim = 50,
                      nombre = "vias")
} # }
```
