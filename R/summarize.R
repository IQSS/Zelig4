#' Generic methonf for summarizing simualted quantities of interest
#' 
#' @S3method summarize default
#'
#' @param qi a `qi' object, storing simulations of quantities of interest
#' @return a `summarized.qi' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summarize <- function(obj)
  UseMethod("summarize")
