#' Set Explanatory Variables for Multiply Imputed Data-sets
#' @export
setx.MI <- function(obj, ..., data = NULL) {

  results.list <- list()

  for (key in names(obj$list)) {
    object <- obj$list[[key]]
    results.list[[key]] <- setx(object, ..., data = data)
  }

  class(results.list) <- c("setx.mi", "setx")
  results.list
}
