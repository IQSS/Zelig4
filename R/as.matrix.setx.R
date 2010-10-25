as.matrix.setx <- function(x) {
  if (!is.null(x$matrix))
    x$matrix
  
  else
    stop("unspecified error")
}
