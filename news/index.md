# Changelog

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
