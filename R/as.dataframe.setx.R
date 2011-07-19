#' Coerce a \code{setx} Object into a \code{data.frame}
#' @usage \method{as.data.frame}{setx}(x, row.names=NULL, optional=FALSE, ...)
#' @note In subsequent versions of Zelig, this version is expected to undergo
#'   minor modifications.
#' @param x a \code{setx} object
#' @param row.names ignored parameter
#' @param optional ignored parameter
#' @param ... ignored parameters
#' @return the \code{setx} object interpretted as a \code{data.frame}. The
#'   column-names of the resulting \code{data.frame} are specified by the names
#'   of the \code{setx} object. The row-names are typically unlabeled.
#' @S3method as.data.frame setx
as.data.frame.setx <- function (x, row.names=NULL, optional=FALSE, ...) {
  x$matrix
}
