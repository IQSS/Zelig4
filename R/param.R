#' Generic Method for Simulating Ancillary/Auxillary Parameters
#' param obj 
#' param ...
#' value 
param <- function (obj, ...)
  UseMethod("param")

param.default <- function (obj, ...)
  NULL
