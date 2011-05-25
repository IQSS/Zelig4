#' Default describe function for an arbitrary model
#' This method exists solely as a backup when an author does not contribute a
#' 'describe' function for their model
#' @S3method describe default
#' @param ... dummy parameters purely to cast the correct object. That is, the
#'   parameters of the function should not
#'            BE referenced specifically
#' @return a list to be processed by \code{as.description}
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.default <- function(...) {
  warning("The 'describe' method for this function is unspecified")
  list(
       authors = "Unknown Author",
       year    = as.numeric(format(Sys.Date(), "%Y"))
       )
}
