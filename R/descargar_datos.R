#' Descarga de los datos de ejemplo
#'
#' @description Descarga los rasters de ejemplo de la Ciudad de Medellin (Colombia) que se utilizan en
#' los ejemplos de la documentacion y en el README. Por su tamano, estos archivos no se incluyen en el
#' paquete: se descargan desde el repositorio del proyecto en GitHub y se guardan en una carpeta de
#' cache del usuario, por lo que solo se descargan la primera vez.
#' @author Juan Pablo Carranza
#' @param archivos Nombres de los archivos a descargar. Por defecto, todos los disponibles
#'   (ver \code{datos_disponibles()}).
#' @param directorio Carpeta en la que se guardan los archivos. Por defecto, la carpeta de cache del
#'   paquete, que se obtiene con \code{tools::R_user_dir("valuate", "cache")}.
#' @param sobrescribir Logical. Si es 'TRUE', los archivos se descargan nuevamente aunque ya existan.
#'
#' @return Un vector con las rutas a los archivos descargados, que puede pasarse directamente a
#'   \code{terra::rast()} o al argumento \code{independientes} de \code{entrenar_modelo()}.
#' @export
#'
#' @examples
#' datos_disponibles()
#' \dontrun{
#' rutas <- descargar_datos(c("dist_tren.tif", "entorno_comercios.tif"))
#' terra::rast(rutas)
#' }
descargar_datos <- function(archivos = datos_disponibles(),
                            directorio = tools::R_user_dir("valuate", "cache"),
                            sobrescribir = FALSE) {
  desconocidos <- setdiff(archivos, datos_disponibles())
  if (length(desconocidos) > 0) {
    stop("Los siguientes archivos no estan disponibles: ", paste(desconocidos, collapse = ", "),
         ". Consulte la lista con datos_disponibles().")
  }
  dir.create(directorio, recursive = TRUE, showWarnings = FALSE)
  rutas <- file.path(directorio, archivos)
  faltantes <- which(!file.exists(rutas) | sobrescribir)
  if (length(faltantes) > 0) {
    timeout_original <- options(timeout = max(600, getOption("timeout")))
    on.exit(options(timeout_original), add = TRUE)
    url_base <- getOption("valuate.url_datos",
                          "https://github.com/r-valuate/valuate/releases/download/v0.1.0/")
    for (i in faltantes) {
      message("Descargando ", archivos[i], "...")
      estado <- tryCatch(
        utils::download.file(paste0(url_base, archivos[i]), rutas[i], mode = "wb", quiet = TRUE),
        error = function(e) 1L
      )
      if (estado != 0) {
        unlink(rutas[i])
        stop("No se pudo descargar ", archivos[i], ". Verifique su conexion a internet.")
      }
    }
  }
  names(rutas) <- archivos
  rutas
}

#' Datos de ejemplo disponibles
#'
#' @description Lista los rasters de ejemplo que pueden descargarse con \code{descargar_datos()}.
#' @author Juan Pablo Carranza
#' @return Un vector con los nombres de los archivos disponibles.
#' @export
#'
#' @examples
#' datos_disponibles()
datos_disponibles <- function() {
  c("dist_basura.tif", "dist_industria.tif", "dist_plazas_parques.tif", "dist_tren.tif",
    "dist_vias_autopista.tif", "dist_vias_prim.tif", "dist_vias_sec.tif", "edificaciones.tif",
    "entorno_altura.tif", "entorno_comercios.tif", "entorno_edificaciones.tif",
    "entorno_hoteles.tif", "entorno_plazas_parques.tif")
}
