#' Coerce a \code{setx} Object into a \code{data.frame}
#'
#' @S3method as.data.frame setx
as.data.frame.setx <- function (x, row.names=NULL, optional=FALSE, ...) {
  x$matrix
}
