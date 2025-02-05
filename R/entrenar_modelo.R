#' Entrenamiento de modelos y predicción del valor
#'
#' @description Esta función permite entrenar modelos y cuantificar su error en la predicción del valor de los inmuebles (o cualquier otra variable territorial que se esté analizando).
#' @author Juan Pablo Carranza
#' @param df Dataframe sobre el que se realizará el cálculo. Debe ser un objeto de la librería 'sf' con geometría 'POINT'.
#' @param dependiente Nombre de la variable a estimar o variable dependiente.
#' @param independientes Vector con los nombres de los rásters que contienen información sobre las variables independientes, generadas a partir de las funciones calcular_dist(), calcular_raster() o calcular_entorno().
#' @param modelo Nombre del modelo a entrenar. Se corresponde con las abrevaciones previstas en la librería 'caret' (Kuhn, 2008) (ver \link{https://topepo.github.io/caret/train-models-by-tag.html}). El valor por default ("qrf") se corresponde al modelo Quantile Regression Forest (Meinshausen, 2006).
#' @param umbral Valor que define el error máximo permitido en la estimación. La función evalúa la precisión fuera de la muestra mediante un proceso de validación cruzada en 10 grupos. Para cada dato en la muestra se registra el error fuera de la muestra. Si el nivel de error agregado es superior al umbral definido, la función procede a eliminar los datos con un nivel de error mayor o igual al definido por el parámetro 'eliminar' (ver abajo).
#' @param eliminar Valor que define el nivel de error a partir del cual se depurará la muestra, siembre que el error general de estimación se encuentre por encima del definido por el parámetro 'umbral'.
#'
#' @return
#' Esta función implementa un flujo completo de entrenamiento, evaluación y ajuste de un modelo supervisado para la estimación de una variable dependiente a partir de un conjunto de variables independientes en formato ráster. Específicamente, está diseñada para trabajar con datos espaciales almacenados en un dataframe con geometría de tipo 'POINT', usando herramientas de las librerías 'sf' y 'terra'.
#' El proceso se desarrolla en los siguientes pasos principales:
#'   \enumerate{
#'     \item{Preprocesamiento de datos:} Los rásters de las variables independientes se combinan con el dataframe espacial mediante la extracción de valores correspondientes a los puntos de la geometría. Los datos se depuran eliminando filas con valores faltantes.
#'     \item{Configuración del modelo:} Se define un modelo a través de la librería 'caret', con soporte para una amplia variedad de algoritmos. El modelo predeterminado es Quantile Regression Forest (QRF), aunque se pueden especificar otros métodos soportados por 'caret'.
#'     \item{Entrenamiento iterativo:} La función utiliza validación cruzada de 10 particiones para evaluar el error medio absoluto porcentual (MAPE) fuera de la muestra. Si el MAPE promedio supera el umbral especificado, se eliminan observaciones cuyo MAPE individual sea igual o superior al 40\%, y el modelo se entrena nuevamente con los datos restantes. Este proceso se repite hasta que el MAPE promedio cumpla con el umbral definido.
#'     \item{Generación de predicción espacial:} Una vez finalizado el entrenamiento, el modelo entrenado se utiliza para interpolar las predicciones sobre los rásters de entrada, generando un nuevo ráster de salida que se guarda en el directorio de trabajo con el nombre 'vut.tif'.
#'     \item{Salida de datos:} La función genera dos conjuntos de datos en el entorno global: \code{datos_utilizados} (datos que cumplieron con el umbral de error) y \code{datos_eliminados} (datos descartados por errores altos).}
#' La función está optimizada para aplicaciones que requieren análisis espaciales precisos y está diseñada para integrarse en flujos de trabajo geoespaciales complejos. Además, emplea paralelización para acelerar el proceso de entrenamiento en máquinas con múltiples núcleos.
#' El diseño de esta función está orientado a contextos donde es crucial garantizar una alta precisión en la estimación, especialmente en aplicaciones que involucran modelización espacial o análisis urbano. Además, facilita la integración con flujos de trabajo basados en datos geoespaciales mediante las librerías 'sf' y 'raster'.
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
#' # A continuación se presenta un ejemplo para la Ciudad de Medellín (Colombia), con sólo 500 datos.
#' load("data/datos.Rda")
#'
#' entrenar_modelo(df = dat,
#'                 dependiente = "vut",
#'                 independientes = c("data/dist_basura.tif",
#'                                    "data/dist_industria.tif",
#'                                    "data/dist_plazas_parques.tif",
#'                                    "data/dist_tren.tif",
#'                                    "data/dist_vias_autopista.tif",
#'                                    "data/dist_vias_prim.tif",
#'                                    "data/dist_vias_sec.tif",
#'                                    "data/entorno_altura.tif",
#'                                    "data/entorno_plazas_parques.tif",
#'                                    "data/entorno_comercios.tif",
#'                                    "data/entorno_edificaciones.tif",
#'                                    "data/entorno_hoteles.tif"),
#'                 modelo = "qrf",
#'                 umbral = 0.2)
#'
entrenar_modelo <- function(df, dependiente, independientes, modelo="qrf", umbral=0.3, eliminar=0.4) {
        suppressPackageStartupMessages(library(terra))
        suppressPackageStartupMessages(library(sf))
        suppressPackageStartupMessages(library(tidyverse))
        df$ID = 1:nrow(df)
        form = paste0(dependiente, " ~ ", paste(sub(".tif", "", independientes), collapse=' + '))
        pred = raster::stack(independientes)
        variable <- terra::extract(rast(pred), terra::vect(df))
        df = suppressMessages(dplyr::left_join(df, variable, by = "ID"))
        df = na.omit(df)
        mapeSummary <- function (data, lev = NULL, model = NULL) {
          mape <- function(y, yhat) mean(abs((y - yhat)/y)) * (-1)
          out <- mape(data$obs, data$pred)
          names(out) <- "MAPE"
          out
        }
        suppressPackageStartupMessages(library(caret))
        fitControl <- caret::trainControl(method = "cv", number = 10,
                                          summaryFunction = mapeSummary,
                                          savePredictions = 'final')
        suppressPackageStartupMessages(library(parallel))
        suppressPackageStartupMessages(library(doParallel))
        cores = detectCores()
        cl <- makeCluster(cores[1])
        registerDoParallel(cl)
        df_qrf = sf::st_drop_geometry(df)
        message("...entrenando modelo...")
        repeat{
                df_qrf$id = 1:nrow(df_qrf)
                set.seed(11)
                qrf <- train(as.formula(form),
                             data = df_qrf,
                             preProcess = c("center","scale"),
                             method = modelo,
                             trControl = fitControl,
                             metric = "MAPE")
                predicciones = qrf$pred
                predicciones = predicciones[,c("rowIndex","pred","obs")]
                predicciones$mape = abs(predicciones$pred - predicciones$obs) / predicciones$obs
                        if (mean(predicciones$mape) < umbral) {
                          break
                          message("El entrenamiento ha finalizado.")
                          }
                predicciones = rename(predicciones, id = rowIndex)
                df_qrf = suppressMessages(left_join(df_qrf, predicciones[,c("mape","id")]))
                a = nrow(df_qrf)
                df_qrf = subset(df_qrf, mape < eliminar)
                message(paste0("El MAPE por el momento es igual a +/- ", round(mean(predicciones$mape)*100,2), "%. Se eliminarán ", nrow(df_qrf) - a, " observaciones. El proceso continúa con ", nrow(df_qrf), " datos."))
        }
        parallel::stopCluster(cl)
        message(paste0("El proceso de entrenamiento del modelo ha finalizado. El MAPE resultó igual a +/- ", round(mean(predicciones$mape)*100,2), "%."))
        df_qrf$condicion = "usado"
        df = suppressMessages(dplyr::left_join(df, df_qrf[,c("condicion","ID")]))
        df_utilizados = subset(df, condicion == "usado")
        df_utilizados$condicion = NULL; df_utilizados$ID = NULL
        df_eliminados = subset(df, is.na(condicion) == TRUE)
        df_eliminados$condicion = NULL; df_eliminados$ID = NULL
        assign("datos_utilizados", df_utilizados, envir=globalenv())
        assign("datos_eliminados", df_eliminados, envir=globalenv())
        message("...realizando interpolación...")
        vut = terra::interpolate(rast(pred), qrf, na.rm = TRUE, wopt=list(steps=80))
        terra::plot(vut, breaks = terra::global(vut, quantile, probs = seq(0,1,0.1), na.rm=TRUE), smooth = TRUE)
        writeRaster(vut, "vut.tif", overwrite = TRUE)
        save(qrf, file = "modelo.rda")
        message("En el directorio de trabajo se ha guardado un ráster con la predicción, con el nombre 'vut.tif'.")
        message("En el directorio de trabajo se ha guardado un archivo con el modelo aplicado, con el nombre 'modelo.rda'.")
}
