#' Entrenamiento de modelos y prediccion del valor
#'
#' @description Esta funcion permite entrenar modelos y cuantificar su error en la prediccion del valor de los inmuebles (o cualquier otra variable territorial que se este analizando).
#' @author Juan Pablo Carranza
#' @param df Dataframe sobre el que se realizara el calculo. Debe ser un objeto de la libreria 'sf' con geometria 'POINT'.
#' @param dependiente Nombre de la variable a estimar o variable dependiente.
#' @param independientes Vector con los nombres de los rasters que contienen informacion sobre las variables independientes, generadas a partir de las funciones calcular_dist(), calcular_raster() o calcular_entorno().
#' @param modelo Nombre del modelo a entrenar. Se corresponde con las abreviaciones previstas en la libreria 'caret' (Kuhn, 2008) (ver \link{https://topepo.github.io/caret/train-models-by-tag.html}). El valor por default ("qrf") se corresponde al modelo Quantile Regression Forest (Meinshausen, 2006).
#' @param umbral Valor que define el error maximo permitido en la estimacion. La funcion evalua la precision fuera de la muestra mediante un proceso de validacion cruzada en 10 grupos. Para cada dato en la muestra se registra el error fuera de la muestra. Si el nivel de error agregado es superior al umbral definido, la funcion procede a eliminar los datos con un nivel de error mayor o igual al definido por el parametro 'eliminar' (ver abajo).
#' @param eliminar Valor que define el nivel de error a partir del cual se depurara la muestra, siempre que el error general de estimacion se encuentre por encima del definido por el parametro 'umbral'.
#'
#' @return
#' Esta funcion implementa un flujo completo de entrenamiento, evaluacion y ajuste de un modelo supervisado para la estimacion de una variable dependiente a partir de un conjunto de variables independientes en formato raster. Especificamente, esta disenada para trabajar con datos espaciales almacenados en un dataframe con geometria de tipo 'POINT', usando herramientas de las librerias 'sf' y 'terra'.
#' El proceso se desarrolla en los siguientes pasos principales:
#'   \enumerate{
#'     \item{Preprocesamiento de datos:} Los rasters de las variables independientes se combinan con el dataframe espacial mediante la extraccion de valores correspondientes a los puntos de la geometria. Los datos se depuran eliminando filas con valores faltantes.
#'     \item{Configuracion del modelo:} Se define un modelo a traves de la libreria 'caret', con soporte para una amplia variedad de algoritmos. El modelo predeterminado es Quantile Regression Forest (QRF), aunque se pueden especificar otros metodos soportados por 'caret'.
#'     \item{Entrenamiento iterativo:} La funcion utiliza validacion cruzada de 10 particiones para evaluar el error medio absoluto porcentual (MAPE) fuera de la muestra. Si el MAPE promedio supera el umbral especificado, se eliminan observaciones cuyo MAPE individual sea igual o superior al 40\%, y el modelo se entrena nuevamente con los datos restantes. Este proceso se repite hasta que el MAPE promedio cumpla con el umbral definido.
#'     \item{Generacion de prediccion espacial:} Una vez finalizado el entrenamiento, el modelo entrenado se utiliza para interpolar las predicciones sobre los rasters de entrada, generando un nuevo raster de salida que se guarda en el directorio de trabajo con el nombre 'vut.tif'.
#'     \item{Salida de datos:} La funcion genera dos conjuntos de datos en el entorno global: \code{datos_utilizados} (datos que cumplieron con el umbral de error) y \code{datos_eliminados} (datos descartados por errores altos).}
#' La funcion esta optimizada para aplicaciones que requieren analisis espaciales precisos y esta disenada para integrarse en flujos de trabajo geoespaciales complejos. Ademas, emplea paralelizacion para acelerar el proceso de entrenamiento en maquinas con multiples nucleos.
#' El diseno de esta funcion esta orientado a contextos donde es crucial garantizar una alta precision en la estimacion, especialmente en aplicaciones que involucran modelizacion espacial o analisis urbano. Ademas, facilita la integracion con flujos de trabajo basados en datos geoespaciales mediante las librerias 'sf' y 'raster'.
#'
#' @references
#' Kuhn, M. (2008). "Building Predictive Models in R Using the caret Package".
#' \emph{Journal of Statistical Software}, \bold{28}(5), 93–115.
#' https://doi.org/10.18637/jss.v028.i05
#' Disponible en:
#'   \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1538-4632.1995.tb00338.x}.
#'
#' Meinshausen, N. (2006) "Quantile Regression Forests".
#' \emph{Journal of Machine Learning Research}, \bold{7}.
#' Disponible en:
#'   \url{https://jmlr.csail.mit.edu/papers/v7/}.
#'
#' @export
#'
#' @examples
#' # A continuacion se presenta un ejemplo para la Ciudad de Medellin (Colombia), con solo 500 datos.
#' data("datos", package = "valuate")
#'
#' entrenar_modelo(df = dat,
#'                 dependiente = "vut",
#'                 independientes = c(system.file("extdata/dist_basura.tif", package = "valuate"),
#'                                    system.file("extdata/dist_industria.tif", package = "valuate"),
#'                                    system.file("extdata/dist_plazas_parques.tif", package = "valuate"),
#'                                    system.file("extdata/dist_tren.tif", package = "valuate"),
#'                                    system.file("extdata/dist_vias_autopista.tif", package = "valuate"),
#'                                    system.file("extdata/dist_vias_prim.tif", package = "valuate"),
#'                                    system.file("extdata/dist_vias_sec.tif", package = "valuate"),
#'                                    system.file("extdata/entorno_altura.tif", package = "valuate"),
#'                                    system.file("extdata/entorno_plazas_parques.tif", package = "valuate"),
#'                                    system.file("extdata/entorno_comercios.tif", package = "valuate"),
#'                                    system.file("extdata/entorno_edificaciones.tif", package = "valuate"),
#'                                    system.file("extdata/entorno_hoteles.tif", package = "valuate")),
#'                 modelo = "qrf",
#'                 umbral = 0.3)
entrenar_modelo <- function(df, dependiente, independientes, modelo="qrf", umbral=0.3, eliminar=0.4) {
        df$ID = 1:nrow(df)
        a <- paste0(dependiente, " ~ ", paste(trimws(basename(independientes)),collapse=" + "))
        form = gsub(".tif", "", a)
        pred = terra::rast(independientes)
        variable <- terra::extract(pred, terra::vect(df))
        df = suppressMessages(dplyr::left_join(df, variable, by = "ID"))
        df = stats::na.omit(df)
        mapeSummary <- function (data, lev = NULL, model = NULL) {
          mape <- function(y, yhat) mean(abs((y - yhat)/y)) * (-1)
          out <- mape(data$obs, data$pred)
          names(out) <- "MAPE"
          out
        }
        fitControl <- caret::trainControl(method = "cv", number = 10,
                                          summaryFunction = mapeSummary,
                                          savePredictions = 'final',
                                          allowParallel = !(Sys.getenv("GITHUB_ACTIONS") == "true"))
        cores <- parallel::detectCores()
        cl <- parallel::makeCluster(cores[1])
        doParallel::registerDoParallel(cl)
        df_qrf = sf::st_drop_geometry(df)
        message("...entrenando modelo...")
        repeat{
                df_qrf$id = 1:nrow(df_qrf)
                set.seed(11)
                qrf <- caret::train(stats::as.formula(form),
                                    data = df_qrf,
                                    preProcess = c("center","scale"),
                                    method = modelo,
                                    trControl = fitControl,
                                    metric = "MAPE")
                predicciones = qrf$pred
                predicciones = predicciones[,c("rowIndex","pred","obs")]
                predicciones$mape = abs(predicciones$pred - predicciones$obs) / predicciones$obs
                        if (mean(predicciones$mape) < umbral) {
                          message("El entrenamiento ha finalizado.")
                          break
                          }
                predicciones = dplyr::rename(predicciones, id = rowIndex)
                df_qrf = suppressMessages(dplyr::left_join(df_qrf, predicciones[,c("mape","id")]))
                a = nrow(df_qrf)
                df_qrf = subset(df_qrf, predicciones$mape  < eliminar)
                message(paste0("El MAPE por el momento es igual a +/- ", round(mean(predicciones$mape)*100,2), "%. Se eliminarán ", nrow(df_qrf) - a, " observaciones. El proceso continúa con ", nrow(df_qrf), " datos."))
        }
        parallel::stopCluster(cl)
        message(paste0("El MAPE resultó igual a +/- ", round(mean(predicciones$mape)*100,2), "%."))
        df_qrf$condicion = "usado"
        df = suppressMessages(dplyr::left_join(df, df_qrf[,c("condicion","ID")]))
        df_utilizados = subset(df, condicion == "usado")
        df_utilizados$condicion = NULL; df_utilizados$ID = NULL
        df_eliminados = subset(df, is.na(condicion) == TRUE)
        df_eliminados$condicion = NULL; df_eliminados$ID = NULL
        assign("datos_utilizados", df_utilizados, envir=globalenv())
        assign("datos_eliminados", df_eliminados, envir=globalenv())
        message("...realizando interpolación...")
        vut = terra::interpolate(pred, qrf, na.rm = TRUE, wopt=list(steps=80))
        terra::plot(vut, breaks = terra::global(vut, quantile, probs = seq(0,1,0.1), na.rm=TRUE), smooth = TRUE)
        terra::writeRaster(vut, "vut.tif", overwrite = TRUE)
        save(qrf, file = "modelo.rda")
        message("En el directorio de trabajo se ha guardado un ráster con la predicción, con el nombre 'vut.tif'.")
        message("En el directorio de trabajo se ha guardado un archivo con el modelo aplicado, con el nombre 'modelo.rda'.")
}
