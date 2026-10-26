# Identificacion de valores espacialmente atipicos

Esta funcion permite eliminar observaciones cuyo valor es muy diferente
a los observados en el entorno. Permite identificar outliers espaciales
mediante el calculo del indice de Moran local, o mediante el promedio
observado en el entorno ponderado por la distancia.

## Usage

``` r
eliminar_outliers(df, dist, variable, umbral, moran)
```

## Arguments

- df:

  Dataframe sobre el que se realizara el calculo. Debe ser un objeto de
  la libreria 'sf' con geometria 'POINT'.

- dist:

  Distancia euclidiana que define la extension del vecindario en el que
  se compararan los valores.

- variable:

  Nombre de la variable sobre la que se realizara el calculo.

- umbral:

  Umbral para caracterizar a una observacion como atipica en relacion a
  lo observado en el vecindario. Por ejemplo, un umbral igual a 0.1
  implicara que se caracterizaran como outliers a todas aquellas
  observaciones cuyo valor este por encima o por debajo del observado en
  el vecindario en una magnitud igual a +/- 10%.

- moran:

  Logical. Si asume el valor 'TRUE' se procede a identificar outliers
  mediante el calculo del indice de Moran local. En este caso, el
  parametro 'umbral' queda sin efecto.

## Value

Un objeto 'sf' con las observaciones de la muestra original, sin las
identificadas como espacialmente atipicas. La funcion permite
identificar valores que son muy diferentes a los observados en el
entorno. Para ello, compara el valor de cada observacion con el promedio
del entorno (ponderado por la distancia, mediante la construccion de una
matriz de pesos espaciales). Tambien permite identificar outliers
espaciales mediante el calculo del indice de Moran local, siguiendo a
Anselin (1995).

## References

Anselin, L. (1995). Local Indicators of Spatial Association - LISA.
*Geographical Analysis*, **27**(2), 93-115.
[doi:10.1111/j.1538-4632.1995.tb00338.x](https://doi.org/10.1111/j.1538-4632.1995.tb00338.x)
. Disponible en:
<https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1538-4632.1995.tb00338.x>.

## Author

Juan Pablo Carranza

## Examples

``` r

# Cargamos la base de datos y tomamos las primeras 1500 observaciones
data("resultado", package = "valuate")
muestra <- resultado[1:1500, ]

# Se define la estimacion de observaciones atipicas a partir de la variable 'vut'
# La distancia para el calculo es de 50 metros lineales. Esto define la extension del vecindario.
# El umbral para considerar a una observacion como atipica se define igual a 0.1 (+/- 10\%)
# No se realiza el calculo mediante el indice de Moran local.
depurados <- eliminar_outliers(df = muestra,
                               dist = 50,
                               variable = "vut",
                               umbral = 0.1,
                               moran = FALSE)
#> Se eliminaron 290 observaciones espacialmente atipicas identificadas mediante el promedio ponderado del entorno.

# Si, en cambio, se desea identificar valores atipicos mediante
# el calculo del indice de Moran local:
depurados_moran <- eliminar_outliers(df = muestra,
                                     dist = 50,
                                     variable = "vut",
                                     moran = TRUE)
#> Se eliminaron 0 observaciones espacialmente atipicas identificadas mediante el indice de Moran local.
```
