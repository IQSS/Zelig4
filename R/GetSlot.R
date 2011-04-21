#' Generic method for extracting variables from both
#' S3 and S4 fitted model object
#'
#' @param obj an object of type `zelig'
#' @param key a character-string specifying the name
#'            of the variable to extract
#' @param ... typically ignored parameters
#' @return the value of that extracted object or NULL
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
GetSlot <- function(obj, key, ...)
  UseMethod("GetSlot")
