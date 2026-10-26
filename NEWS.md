# valuate 0.2.0

## Cambios que afectan el uso del paquete

* Las funciones ya no crean objetos en el entorno de trabajo: ahora devuelven
  su resultado, que se guarda con una asignación. Por ejemplo,
  `depurados <- eliminar_outliers(...)`.
    * `calcular_dist()`, `calcular_entorno()`, `calcular_raster()` y
      `simular_escenario()` devuelven un objeto `SpatRaster`.
    * `eliminar_outliers()` devuelve la muestra sin las observaciones atípicas.
    * `calcular_vut()` devuelve una lista con `datos` (la muestra con las
      columnas `vut` y `coef`) y `efectos` (la tabla de efectos estimados).
    * `entrenar_modelo()` devuelve una lista con `prediccion`, `modelo`,
      `utilizados` y `eliminados`.
* Los rásters de ejemplo de Medellín ya no se incluyen en el paquete. Se
  descargan con la nueva función `descargar_datos()` la primera vez que se
  usan. `datos_disponibles()` lista los archivos disponibles.
* El paquete requiere R 4.1.0 o superior.
* `osmdata` y `nominatimlite` pasan a ser dependencias opcionales, ya que solo
  se usan en los ejemplos.

## Correcciones

* `simular_escenario()` usa el ráster indicado en el argumento `original`, en
  lugar de una ruta fija.
* `calcular_vut()` ya no falla cuando los datos no presentan dependencia
  espacial y corresponde aplicar el modelo de mínimos cuadrados ordinarios.
* `eliminar_outliers()` devuelve la muestra depurada también cuando se usa el
  índice de Moran local, y funciona aunque el paquete `sf` no esté cargado.
* `calcular_raster()` nombra la capa resultante según el argumento `nombre`.

## Otros cambios

* El paquete instalado pasa de 49 MB a menos de 1 MB.
* Se eliminaron archivos de datos que no se utilizaban.
* Nuevos tests para todas las funciones.
