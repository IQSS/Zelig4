#' Generic methonf for summarizing simualted quantities of interest
#' 
#' @S3method summarize default
#'
#' @param obj a \code{qi} object, storing simulations of quantities of interest
#' @return a \code{summarized.qi} object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summarize <- function(obj)
  UseMethod("summarize")
