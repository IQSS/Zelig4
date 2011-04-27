# This file contains helper classes for Zelig

#' Create a Zelig Object from an 'MI' Object
#' @note This function is exclusively used internally by Zelig
#' @param parent the parent model (MI)
#' @param fitted the fitted model object
#' @param data a new data-frame
#' @return a zelig object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig.kin <- function(parent, fitted, data=NULL) {
  # build a list in an obvious way
  z <- list(name = parent$name,
            formula = parent$formula,
            result = fitted,
            args = parent$args,
            data = data,
            call = parent$call,
            is.MI = F,
            by = NULL,
            mi = iter(list()),
            model.obj = parent$model.obj
            )

  # make zelig object, and return
  class(z) <- c("zelig", z$name)
  z  
}
