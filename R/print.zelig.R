#' Print values of `zelig' objects
#' @S3method print zelig
#' @param x a `zelig' object
#' @param ... ignored parameters
#' @return the `zelig' object (invisibly)
#' @export 
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.zelig <- function(x, ...) {
  class(x) <- "list"
  print(x)
}
