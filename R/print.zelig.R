#' Print values of `zelig' objects
#' @S3method print zelig
#' @param x a `zelig' object
#' @param ... ignored parameters
#' @return the `zelig' object (invisibly)
#' @export 
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.zelig <- function(x, ...) {
  message()
  cat("name = ")
  message(x$name)

  cat("arguments = ")
  message(paste(x$args, collapse=", "))

  cat("data columns = ")
  message(paste(colnames(x$data), collapse=", "))

  message("result object")
  print(summary(x$result))
  invisble(x)
}
