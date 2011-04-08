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

#' Indexing operator for MI objects (subtype of `zelig')
#'
#' This is currently not supported, and included for use
#' in future versions
#'
#' @param obj a 'zelig' and 'MI' object
#' @param key a character-string specifying the name
#'            of the variable to extract
#' @param ... ignored parameters
#' @return stops before it can return anything (as of 4/4/2011)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
"[[.MI" <- function(obj, key, ...)
  stop("under construction")

