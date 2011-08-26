#' Set Explanatory Variables for Multiply Imputed Data-sets
#' This function simply calls setx.default once for every fitted model
#' within the 'zelig.MI' object
#' @usage \method{setx}{MI}(obj, ..., data=NULL)
#' @S3method setx MI
#' @param obj a 'zelig' object
#' @param ... user-defined values of specific variables for overwriting the
#'   default values set by the function \code{fn}
#' @param data a new data-frame
#' @return a 'setx.mi' object used for computing Quantities of Interest by the
#'   'sim' method
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @seealso \link{setx}
setx.MI <- function(obj, ..., data = NULL) {

  results.list <- list()

  for (key in names(obj)) {
    object <- obj[[key]]
    results.list[[key]] <- setx(object, ..., data = data)
  }

  class(results.list) <- c("setx.mi", "setx")
  results.list
}
