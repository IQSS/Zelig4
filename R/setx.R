#' Generic Function for Setting Explanatory Variables and Counterfactuals
#' param obj 
#' param ...
#' param data 
#' value a setx object
setx <- function(obj, ..., data=NULL)
  UseMethod("setx")
