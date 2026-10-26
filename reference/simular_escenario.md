# Simulacion de escenarios y estimacion de externalidades espaciales

A partir de la prediccion de la distribucion espacial de una variable,
la funcion permite estimar escenarios a partir de la simulacion de
alteraciones en alguna (o algunas) de las variables independientes.

## Usage

``` r
simular_escenario(modelo, independientes, original)
```

## Arguments

- modelo:

  Archivo .rda guardado en el directorio de trabajo por la funcion
  entrenar_modelo().

- independientes:

  Vector con los nombres de las variables independientes. Debe tener los
  mismos nombre que los utilizados en la funcion entrenar_modelo(), solo
  que admite que los rasters tengan una distribucion espacial diferente,
  incorporando modificaciones simuladas a la nueva prediccion.

- original:

  Resultado del modelo original. Es un archivo .tif guardado en el
  directorio de trabajo por la funcion entrenar_modelo().

## Value

Un objeto 'SpatRaster' (libreria 'terra'), que tambien se guarda en el
directorio de trabajo con el nombre 'impacto.tif'. La funcion devuelve
un raster que informa sobre el impacto en la variable dependiente de las
simulaciones realizadas en las variables independientes. Permite estimar
la magnitud y el alcance territorial de las modificaciones simuladas
sobre la variable de estudio.

## Author

Juan Pablo Carranza
