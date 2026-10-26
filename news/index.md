# Changelog

## valuate 0.2.1

### Correcciones

- [`entrenar_modelo()`](https://r-valuate.github.io/valuate/reference/entrenar_modelo.md):
  cuando el error superaba el umbral, el proceso de depuración eliminaba
  observaciones equivocadas (no necesariamente las de error alto),
  porque comparaba los errores en un orden distinto al de los datos.
  Ahora se eliminan exactamente las observaciones con error mayor o
  igual a `eliminar`. Esto puede cambiar los resultados obtenidos con
  versiones anteriores cuando el ciclo de depuración se activaba.
- [`entrenar_modelo()`](https://r-valuate.github.io/valuate/reference/entrenar_modelo.md):
  si el error sigue por encima del umbral pero ya no quedan
  observaciones para eliminar, el proceso se detiene con un mensaje en
  lugar de repetirse indefinidamente. El mensaje informa correctamente
  la cantidad de observaciones eliminadas en cada vuelta.
- [`calcular_vut()`](https://r-valuate.github.io/valuate/reference/calcular_vut.md):
  con versiones recientes de `spdep` (1.3-3 o posteriores), que
  renombraron los tests robustos a `adjRSerr` y `adjRSlag`, la función
  nunca elegía los modelos LAG ni de error espacial: cuando el test
  SARMA no era significativo aplicaba siempre mínimos cuadrados
  ordinarios. Ahora funciona con ambas versiones de `spdep`.

### Otros cambios

- [`calcular_vut()`](https://r-valuate.github.io/valuate/reference/calcular_vut.md)
  usa una función interna más simple para crear las variables
  indicadoras, en lugar de una copia de `fastDummies::dummy_cols()`. Los
  resultados son idénticos.
- Cobertura de tests del 100%, incluidos los modelos LAG y de error
  espacial de
  [`calcular_vut()`](https://r-valuate.github.io/valuate/reference/calcular_vut.md).

## valuate 0.2.0

### Cambios que afectan el uso del paquete

- Las funciones ya no crean objetos en el entorno de trabajo: ahora
  devuelven su resultado, que se guarda con una asignación. Por ejemplo,
  `depurados <- eliminar_outliers(...)`.
  - [`calcular_dist()`](https://r-valuate.github.io/valuate/reference/calcular_dist.md),
    [`calcular_entorno()`](https://r-valuate.github.io/valuate/reference/calcular_entorno.md),
    [`calcular_raster()`](https://r-valuate.github.io/valuate/reference/calcular_raster.md)
    y
    [`simular_escenario()`](https://r-valuate.github.io/valuate/reference/simular_escenario.md)
    devuelven un objeto `SpatRaster`.
  - [`eliminar_outliers()`](https://r-valuate.github.io/valuate/reference/eliminar_outliers.md)
    devuelve la muestra sin las observaciones atípicas.
  - [`calcular_vut()`](https://r-valuate.github.io/valuate/reference/calcular_vut.md)
    devuelve una lista con `datos` (la muestra con las columnas `vut` y
    `coef`) y `efectos` (la tabla de efectos estimados).
  - [`entrenar_modelo()`](https://r-valuate.github.io/valuate/reference/entrenar_modelo.md)
    devuelve una lista con `prediccion`, `modelo`, `utilizados` y
    `eliminados`.
- Los rásters de ejemplo de Medellín ya no se incluyen en el paquete. Se
  descargan con la nueva función
  [`descargar_datos()`](https://r-valuate.github.io/valuate/reference/descargar_datos.md)
  la primera vez que se usan.
  [`datos_disponibles()`](https://r-valuate.github.io/valuate/reference/datos_disponibles.md)
  lista los archivos disponibles.
- El paquete requiere R 4.1.0 o superior.
- `osmdata` y `nominatimlite` pasan a ser dependencias opcionales, ya
  que solo se usan en los ejemplos.

### Correcciones

- [`simular_escenario()`](https://r-valuate.github.io/valuate/reference/simular_escenario.md)
  usa el ráster indicado en el argumento `original`, en lugar de una
  ruta fija.
- [`calcular_vut()`](https://r-valuate.github.io/valuate/reference/calcular_vut.md)
  ya no falla cuando los datos no presentan dependencia espacial y
  corresponde aplicar el modelo de mínimos cuadrados ordinarios.
- [`eliminar_outliers()`](https://r-valuate.github.io/valuate/reference/eliminar_outliers.md)
  devuelve la muestra depurada también cuando se usa el índice de Moran
  local, y funciona aunque el paquete `sf` no esté cargado.
- [`calcular_raster()`](https://r-valuate.github.io/valuate/reference/calcular_raster.md)
  nombra la capa resultante según el argumento `nombre`.

### Otros cambios

- El paquete instalado pasa de 49 MB a menos de 1 MB.
- Se eliminaron archivos de datos que no se utilizaban.
- Nuevos tests para todas las funciones.
