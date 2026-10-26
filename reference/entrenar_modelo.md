# Entrenamiento de modelos y prediccion del valor

Esta funcion permite entrenar modelos y cuantificar su error en la
prediccion del valor de los inmuebles (o cualquier otra variable
territorial que se este analizando).

## Usage

``` r
entrenar_modelo(
  df,
  dependiente,
  independientes,
  modelo = "qrf",
  umbral = 0.3,
  eliminar = 0.4
)
```

## Arguments

- df:

  Dataframe sobre el que se realizara el calculo. Debe ser un objeto de
  la libreria 'sf' con geometria 'POINT'.

- dependiente:

  Nombre de la variable a estimar o variable dependiente.

- independientes:

  Vector con los nombres de los rasters que contienen informacion sobre
  las variables independientes, generadas a partir de las funciones
  calcular_dist(), calcular_raster() o calcular_entorno().

- modelo:

  Nombre del modelo a entrenar. Se corresponde con las abreviaciones
  previstas en la libreria 'caret' (Kuhn, 2008) (ver
  <https://topepo.github.io/caret/train-models-by-tag.html>). El valor
  por default ("qrf") se corresponde al modelo Quantile Regression
  Forest (Meinshausen, 2006).

- umbral:

  Valor que define el error maximo permitido en la estimacion. La
  funcion evalua la precision fuera de la muestra mediante un proceso de
  validacion cruzada en 10 grupos. Para cada dato en la muestra se
  registra el error fuera de la muestra. Si el nivel de error agregado
  es superior al umbral definido, la funcion procede a eliminar los
  datos con un nivel de error mayor o igual al definido por el parametro
  'eliminar' (ver abajo).

- eliminar:

  Valor que define el nivel de error a partir del cual se depurara la
  muestra, siempre que el error general de estimacion se encuentre por
  encima del definido por el parametro 'umbral'.

## Value

Una lista (devuelta de forma invisible) con cuatro elementos:
`prediccion`, un objeto 'SpatRaster' con la prediccion espacial;
`modelo`, el modelo entrenado; `utilizados`, los datos que cumplieron
con el umbral de error; y `eliminados`, los datos descartados por
errores altos. Ademas, la prediccion y el modelo se guardan en el
directorio de trabajo con los nombres 'vut.tif' y 'modelo.rda'.

Esta funcion implementa un flujo completo de entrenamiento, evaluacion y
ajuste de un modelo supervisado para la estimacion de una variable
dependiente a partir de un conjunto de variables independientes en
formato raster. Especificamente, esta disenada para trabajar con datos
espaciales almacenados en un dataframe con geometria de tipo 'POINT',
usando herramientas de las librerias 'sf' y 'terra'. El proceso se
desarrolla en los siguientes pasos principales:

1.  Preprocesamiento de datos: Los rasters de las variables
    independientes se combinan con el dataframe espacial mediante la
    extraccion de valores correspondientes a los puntos de la geometria.
    Los datos se depuran eliminando filas con valores faltantes.

2.  Configuracion del modelo: Se define un modelo a traves de la
    libreria 'caret', con soporte para una amplia variedad de
    algoritmos. El modelo predeterminado es Quantile Regression Forest
    (QRF), aunque se pueden especificar otros metodos soportados por
    'caret'.

3.  Entrenamiento iterativo: La funcion utiliza validacion cruzada de 10
    particiones para evaluar el error medio absoluto porcentual (MAPE)
    fuera de la muestra. Si el MAPE promedio supera el umbral
    especificado, se eliminan observaciones cuyo MAPE individual sea
    igual o superior al 40%, y el modelo se entrena nuevamente con los
    datos restantes. Este proceso se repite hasta que el MAPE promedio
    cumpla con el umbral definido.

4.  Generacion de prediccion espacial: Una vez finalizado el
    entrenamiento, el modelo entrenado se utiliza para interpolar las
    predicciones sobre los rasters de entrada, generando un nuevo raster
    de salida que se guarda en el directorio de trabajo con el nombre
    'vut.tif'.

5.  Salida de datos: La funcion devuelve los datos que cumplieron con el
    umbral de error (`utilizados`) y los descartados por errores altos
    (`eliminados`).

La funcion esta optimizada para aplicaciones que requieren analisis
espaciales precisos y esta disenada para integrarse en flujos de trabajo
geoespaciales complejos. Ademas, emplea paralelizacion para acelerar el
proceso de entrenamiento en maquinas con multiples nucleos. El diseno de
esta funcion esta orientado a contextos donde es crucial garantizar una
alta precision en la estimacion, especialmente en aplicaciones que
involucran modelizacion espacial o analisis urbano. Ademas, facilita la
integracion con flujos de trabajo basados en datos geoespaciales
mediante las librerias 'sf' y 'raster'.

## References

Kuhn, M. (2008). "Building Predictive Models in R Using the caret
Package". *Journal of Statistical Software*, **28**(5), 93-115.
https://doi.org/10.18637/jss.v028.i05 Disponible en:
<https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1538-4632.1995.tb00338.x>.

Meinshausen, N. (2006) "Quantile Regression Forests". *Journal of
Machine Learning Research*, **7**. Disponible en:
<https://jmlr.csail.mit.edu/papers/v7/>.

## Author

Juan Pablo Carranza

## Examples

``` r
if (FALSE) { # \dontrun{
# A continuacion se presenta un ejemplo para la Ciudad de Medellin (Colombia), con solo 500 datos.
data("datos", package = "valuate")
rasters <- c("dist_basura.tif", "dist_industria.tif", "dist_plazas_parques.tif",
             "dist_tren.tif", "dist_vias_autopista.tif", "dist_vias_prim.tif",
             "dist_vias_sec.tif", "entorno_altura.tif", "entorno_plazas_parques.tif",
             "entorno_comercios.tif", "entorno_edificaciones.tif", "entorno_hoteles.tif")
modelo <- entrenar_modelo(df = dat,
                          dependiente = "vut",
                          independientes = descargar_datos(rasters),
                          modelo = "qrf",
                          umbral = 0.3)

modelo$prediccion
modelo$utilizados
} # }
```
