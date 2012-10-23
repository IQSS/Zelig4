#' @S3method vcov gee.naive
vcov.gee.naive <- function(object, ...)
  object$naive.variance

#' @S3method vcov gee.robust
vcov.gee.robust <- function(object, ...)
  object$robust.variance

#' @S3method vcov glm.robust
vcov.glm.robust <- function(object, ...) {
  so <- summary.glm.robust(object, corr=FALSE, ...)
  so$dispersion * so$cov.unscaled
}

#' @S3method vcov Relogit
vcov.Relogit <- function(object, ...) 
  summary.Relogit(object, ...)$cov.scaled
