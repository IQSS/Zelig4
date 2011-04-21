#' Print values of multiply-imputed `setx' objects
#'
#' @S3method print setx.mi
#' 
#' @param x a `setx.mi' object
#' @param ... ignored parameters
#' @return NULL
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.setx.mi <- function(x, ...) {
   blah <- x

   class(blah) <- "list"
   cat("[setx]\n")
   print(blah)
  #

}
