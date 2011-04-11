#' Generic Function for Setting Explanatory Variables and Counterfactuals
#' @param obj 
#' @param ...
#' @param data 
#' @return a setx object
#' @export
#' @author Matt Owen and Olivia Lau and Kosuke Imai \email{mowen@@iq.harvard.edu}
setx <- function(obj, ..., data=NULL)
  UseMethod("setx")
