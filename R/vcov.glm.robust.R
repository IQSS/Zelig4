#' Return Variance-Covariance Matrix
#'
#' Returns the variance-covariance matrix produced for this model.
#' @S3method vcov glm.robust
#' @param object 
#' @param ...
#' @return a NxN matrix, where N is the number of parameters in the fitted
#' model.
vcov.glm.robust <- function(object, ...) {
  so <- summary.glm.robust(object, corr=FALSE, ...)
  so$dispersion * so$cov.unscaled
}
