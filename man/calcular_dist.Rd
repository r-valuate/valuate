% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcular_dist.R
\name{calcular_dist}
\alias{calcular_dist}
\title{Calculo de variables de distancia}
\usage{
calcular_dist(area, objeto, dim, nombre)
}
\arguments{
\item{area}{Poligono que indica el area sobre la cual se ejecutara el calculo.}

\item{objeto}{Objeto espacial "sf" contra el que se calcularan las distancias.}

\item{dim}{Tamano de cada pixel del raster resultante, en metros.}

\item{nombre}{Nombre de la variable resultante. El raster sera guardado en el entorno de trabajo con este nombre y la extension ".tif".}
}
\value{
Se genera un raster que informa, para cada pixel, sobre la distancia (en metros lineales) al objeto mas cercano.
}
\description{
Esta funcion devuelve un raster con las distancias euclidianas, en metros, desde cada pixel hacia el objeto vectorial mas cercano.
}
\examples{
# Definimos el area de estudio
area_de_estudio <- nominatimlite::geo_lite_sf(address = "La Plata, Argentina", points_only = FALSE)
bbox = sf::st_transform(area_de_estudio, 4326) |> sf::st_bbox(bbox)

# Descargamos las vias principales
vias_prim <- osmdata::opq(bbox) |>
  osmdata::add_osm_feature(key = "highway", value = "secondary") |>
  osmdata::osmdata_sf()
vias_prim <- vias_prim$osm_lines

# Calculamos un raster con las distancias a la via principal mas cercana
calcular_dist(area = area_de_estudio,
              objeto = vias_prim,
              dim = 50,
              nombre = "vias")
}
\author{
Juan Pablo Carranza
}
