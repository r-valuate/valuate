
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Valuate

<!-- badges: start -->

[![R-CMD-check](https://github.com/carranzajuanp/valuate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/carranzajuanp/valuate/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Este paquete provee una serie de funciones orientadas a facilitar el
proceso de valuación de inmuebles, siguiendo el desarrollo de Carranza &
Eguino Lijeron (2025). Sin embargo, puede ser también de utilidad para
la predicción de la distribución espacial de cualquier tipo de fenómeno
geográfico.

El flujo de trabajo propuesto apunta a la generación de herramientas
para facilitar las siguientes acciones:

- Creación de variables territoriales que informan sobre la distancia a
  diferentes hitos o las características de determinados entornos.
- Homogeneización de valores para descontar los efectos de
  características intrínsecas de las observaciones. muestrales. Por
  ejemplo, en el caso de la valuación de la tierra, se descuentan los
  efectos sobre el precio por metro cuadrado de la forma de las
  parcelas, su ubicación en la cuadra o el tamaño del lote.
- Diferentes alternativas para la identificación de observaciones
  muestrales espacialmente atípicas.
- Entrenamiento de diferentes modelos de aprendizaje computacional y
  estimación del nivel de error esperado fuera de la muestra.
- Simulación de escenarios que permiten conocer la magnitud y extensión
  geográfica de diferentes externalidades generadas por modificaciones
  en las variables independientes sobre el precio de los inmuebles.

## Instalación

Para instalar la versión de desarrollo de esta librería en la consola de
R se debe ejecutar el siguiente código:

``` r
remotes::install_github("carranzajuanp/valuate")
```

## Contenidos

La función calcular_dist() permite generar un ráster que informa la
distancia de cada píxel hacia el hito más cercano.

Por ejemplo, si se desea conocer la distancia a la vía primaria más
cercana en la ciudad de La Plata (Argentina):

``` r
# Definimos el área de estudio
library(nominatimlite)
library(osmdata)
#> Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright
library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
area_de_estudio <- geo_lite_sf(address = "La Plata, Argentina", points_only = F)
bbox = st_transform(area_de_estudio, 4326) |> st_bbox(bbox)

# Descargamos las vias principales
vias_prim <- opq(bbox) |>
  add_osm_feature(key = "highway", value = "secondary") |>
  osmdata_sf()
vias_prim <- vias_prim$osm_lines

# Calculamos un ráster con las distancias a la vía principal más cercana
calcular_dist(area = area_de_estudio,
              objeto = vias_prim,
              dim = 50,
              nombre = "vias")
```

<img src="man/figures/README-calcular_dist-1.png" width="100%" />

    #> El proceso de cálculo ha finalizado, y el raster resultante se gardó en el directorio de trabajo con el nombre vias.tif. En el environment se ha creado el ráster 'vias'.

La función calcular_entorno() resume las características de un entorno
definido por el usuario, a partir de capas geográficas vectoriales de
polígonos o de puntos. Cuando se le provee un objeto espacial “sf” con
geometría “POLYGON” o “MULTIPOLYGON” la función permite calcular
variables independientes que resumen las características del vecindario,
indicando la proporción del espacio que se encuentra cubierto por los
polígonos de interés. Por ejemplo, permite calcular el porcentaje del
vecindario que se encuentra cubierto por plazas o parques. Cuando se le
provee un objeto espacial “sf” con geometría “POINT” la función permite
calcular variables independientes que resumen las características del
vecindario, indicando la cantidad de puntos de interés que se encuentran
dentro del entorno definido por el usuario. Por ejemplo, permite
calcular la cantidad de negocios u oficinas que se encuentran dentro de
un radio de “x” metros de cada píxel.

Por ejemplo, si se desea conocer información sobre los espacios verdes
en el vecindario en la ciudad de La Plata (Argentina):

``` r
# Definimos el área de estudio
library(nominatimlite)
library(osmdata)
library(sf)
area_de_estudio <- geo_lite_sf(address = "La Plata, Argentina", points_only = F)
bbox = st_transform(area_de_estudio, 4326) |> st_bbox(bbox)

# Descargamos los parques que se encuentran dentro del área de estudio
plazas <- opq(bbox) |>
  add_osm_feature(key = "leisure", value = "park") |>
  osmdata_sf()
plazas <- plazas$osm_polygons

# Calculamos un ráster que resume, para cada píxel las características del entorno
calcular_entorno(area = area_de_estudio,
                 ext = 500,
                 objeto = plazas,
                 dim = 50,
                 nombre = "plazas_entorno")
#> Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
#> GDAL Error 1: PROJ: proj_as_wkt: DatumEnsemble can only be exported to
#> WKT2:2019
```

<img src="man/figures/README-calcular_entorno-1.png" width="100%" />

    #> [1] "El proceso de cálculo ha finalizado, y el raster resultante se gardó en el directorio de trabajo con el nombre plazas_entorno.tif. En el environment se ha creado el ráster 'plazas_entorno'."

La función calcular_raster() devuelve un ráster que resume el cálculo
realizado a partir de otro ráster, incluyendo fuentes de datos con
diferente proyección y resolución espacial. La función permite readecuar
las resoluciones y proyecciones de diferentes rásters a las utilizadas
en el proyecto. Además, el paŕametro “entorno” permite realizar un
cálculo del promedio de la variable bajo análisis en el vecindario
definido por el usuario. Por ejemplo, si se desea readecuar a los
criterios de un proyecto un ráster que informa sobre el porcentaje de
edificaciones detectadas en un píxel a partir de una imagen satelital en
la ciudad de Medellín (Colombia):

``` r
library(terra)
#> terra 1.8.9
original = rast("data/edificaciones.tif")
library(sf)
area_de_estudio = st_read("data/area.gpkg")
#> Reading layer `area' from data source 
#>   `/home/juan/Documentos/GitHub/valuate/valuate/data/area.gpkg' 
#>   using driver `GPKG'
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -75.66658 ymin: 6.066645 xmax: -75.49011 ymax: 6.367076
#> Geodetic CRS:  WGS 84
calcular_raster(original, area_de_estudio, dim = 50, entorno = 100, "prueba_raster")
#> |---------|---------|---------|---------|=========================================                                          
```

<img src="man/figures/README-calcular_raster-1.png" width="100%" />

    #> El proceso de cálculo ha finalizado, y el raster resultante se guardó en el directorio de trabajo con el nombre prueba_raster.tif. En el environment se ha creado el ráster 'prueba_raster'.

La función calcular_vut() estima el valor de las obervaciones muestrales
expresado en términos homogéneos o comparables. La función estima el
precio por metro cuadrado homogeneizado, siguiendo a Carranza (2024).
Toma como input un objeto espacial ‘sf’ y ajusta coeficientes de
corrección según diferentes variables definidas por el usuario para
reexpresar el precio en término de un ‘lote típico’ para el área de
estudio. La función devuelve un objetivo llamado “resultado”, que
consiste en el df original al cual se agrega una columna correspondiente
al valor homogeneizado (vut) y otra columna con el coeficiente de
homogeneización aplicado (coef). Se informa, además, el parámetro de
ajuste estimado por el modelo lineal para cada una de las variables
independientes involucradas en el proceso de homogeneización.

Por ejemplo, suponga que se desea descontar los efectos sobre el precio
por metro cuadrado de los lotes muestreados en la ciudad de Córdoba
(Argentina) de las siguientes características: si se trata de una oferta
o una venta, si se trata de una operación con o sin escritura, la
superficie de cada lote.

``` r
# Cargamos los datos correspondientes a una muestra de 1000 terrenos.
load("data/terrenos.rda")

# Se define como "lote típico" a aquel que se corresponde a una operación de
# venta, realizada con escritura, y de una superficie igual a la mediana
# de los lotes de la ciudad.
# Se indica que la variable que informa sobre el precio por metro cuadrado
# es "valor_m2".
# Se define al vecindario en base a una distancia euclidiana de 500 metros.

calcular_vut(df = terrenos,
             index = "id",
             independientes = c("tipo_valor" = "Venta",
                                "superficie" = "mediana",
                                "situacion_juridica" = "Con Escritura"),
             dependiente = "valor_m2",
             dist = 500)
#> Se eliminarán 1 observaciones con datos faltantes
#> Se procederá a realizar correcciones por dependencia espacial aplicando un modelo SARMA.
#> Este proceso puede demorar varios minutos, dependiendo de la cantidad de observaciones en la muestra.
#> Se procederá a calcular el valor unitario de referencia...
```

<img src="man/figures/README-calcular_vut-1.png" width="100%" />

    #> El proceso de homogeneización ha finalizado. Se ha¨ agregado al entorno de trabajo un nuevo objeto que contiene la muestra original, más una nueva columna llamada 'vut', que contiene el precio reexpresado en términos homogéneos.
    #> Los efectos estadísticamene significativos fueron los siguientes:

| Variable                              |   p-valor |     Efecto |
|:--------------------------------------|----------:|-----------:|
| superficie                            | 0.0000000 | -0.3770692 |
| situacion_juridicaSin título/Posesión | 0.0026828 | -0.5532533 |

La función entrenar_modelo() permite entrenar modelos y cuantificar su
error en la predicción del valor de los inmuebles (o cualquier otra
variable territorial que se esté analizando). Esta función implementa un
flujo completo de entrenamiento, evaluación y ajuste de un modelo
supervisado para la estimación de una variable dependiente a partir de
un conjunto de variables independientes en formato ráster.
Específicamente, está diseñada para trabajar con datos espaciales
almacenados en un dataframe con geometría de tipo ‘POINT’, usando
herramientas de las librerías ‘sf’ y ‘terra’. El proceso se desarrolla
en los siguientes pasos principales:

- Preprocesamiento de datos: Los rásters de las variables independientes
  se combinan con el dataframe espacial mediante la extracción de
  valores correspondientes a los puntos de la geometría. Los datos se
  depuran eliminando filas con valores faltantes.

- Configuración del modelo: Se define un modelo a través de la librería
  ‘caret’, con soporte para una amplia variedad de algoritmos. El modelo
  predeterminado es Quantile Regression Forest (QRF), aunque se pueden
  especificar otros métodos soportados por ‘caret’.

- Entrenamiento iterativo: La función utiliza validación cruzada de 10
  particiones para evaluar el error medio absoluto porcentual (MAPE)
  fuera de la muestra. Si el MAPE promedio supera el umbral
  especificado, se eliminan observaciones cuyo MAPE individual sea igual
  o superior al 40%, y el modelo se entrena nuevamente con los datos
  restantes. Este proceso se repite hasta que el MAPE promedio cumpla
  con el umbral definido.

- Generación de predicción espacial: Una vez finalizado el
  entrenamiento, el modelo entrenado se utiliza para interpolar las
  predicciones sobre los rásters de entrada, generando un nuevo ráster
  de salida que se guarda en el directorio de trabajo con el nombre
  ‘vut.tif’.

- Salida de datos: La función genera dos conjuntos de datos en el
  entorno global: datos_utilizados (datos que cumplieron con el umbral
  de error) y datos_eliminados (datos descartados por errores altos).

La función está optimizada para aplicaciones que requieren análisis
espaciales precisos y está diseñada para integrarse en flujos de trabajo
geoespaciales complejos. Además, emplea paralelización para acelerar el
proceso de entrenamiento en máquinas con múltiples núcleos. El diseño de
esta función está orientado a contextos donde es crucial garantizar una
alta precisión en la estimación, especialmente en aplicaciones que
involucran modelización espacial o análisis urbano. Además, facilita la
integración con flujos de trabajo basados en datos geoespaciales
mediante las librerías ‘sf’ y ‘raster’.

``` r
# A continuación se presenta un ejemplo para la Ciudad de Medellín (Colombia), con sólo 500 datos.
load("data/datos.Rda")

entrenar_modelo(df = dat,
                dependiente = "vut",
                independientes = c("dist_basura.tif",
                                   "dist_industria.tif",
                                   "dist_plazas_parques.tif",
                                   "dist_tren.tif",
                                   "dist_vias_autopista.tif",
                                   "dist_vias_prim.tif",
                                   "dist_vias_sec.tif",
                                   "entorno_altura.tif",
                                   "entorno_plazas_parques.tif",
                                   "entorno_comercios.tif",
                                   "entorno_edificaciones.tif",
                                   "entorno_hoteles.tif"),
                modelo = "qrf",
                umbral = 0.2)
#> ...entrenando modelo...
#> El MAPE por el momento es igual a +/- 28.67%. Se eliminarán -89 observaciones. El proceso continúa con 389 datos.
#> El proceso de entrenamiento del modelo ha finalizado. El MAPE resultó igual a +/- 14.46%.
#> ...realizando interpolación...
#> |---------|---------|---------|---------|=========================================                                          
```

<img src="man/figures/README-entrenar_modelo-1.png" width="100%" />

    #> En el directorio de trabajo se ha guardado un ráster con la predicción, con el nombre 'vut.tif'.
    #> En el directorio de trabajo se ha guardado un archivo con el modelo aplicado, con el nombre 'modelo.rda'.

Finalmente, la función simular_escenario() permite, a partir de la
predicción de la distribución espacial de una variable, estimar
escenarios a partir de la simulación de alteraciones en alguna (o
algunas) de las variables independientes. La función devuelve un ráster
que informa sobre el impacto en la variable dependiente de las
simulaciones realizadas en las variables independientes. Permite estimar
la magnitud y el alcance territorial de las modificaciones simuladas
sobre la variable de estudio.

## Referencias

Carranza J.P. & Eguino Lijeron J.H. (2025). “Handbook for mass valuation
policies”. Internarional Development Bank. Washington, DC.

Carranza, J.P. (2024). “Inteligencia artificial aplicada a políticas de
valuación masiva de la tierra urbana”. Tesis de Doctorado en
Administración y Política Pública. IIFAP, Facultad de Ciencias Sociales,
Universidad Nacional de Córdoba. Córdoba, Argentina.
