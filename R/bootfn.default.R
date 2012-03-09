#' Default Boot-strapping procedure
#' 
#' The default procedure for extracting bootstrap information. Note that this
#' method re-fits the data and resamples the data frequently. This is a good
#' candidate for fixing-up.
#' @param data a data.frame
#' @param i an integer or chacter-string specifying the index of the row to
#' be used in the bootstrapping procedure.
#' @param object the fitted model object
#' @return a list of paramters
bootfn.default <- function(data, i, object) {

  # Extract only the 
  d <- data[i,]

  # Change the data.frame...
  object$call$data <- d

  # Get number of simulations? This seems circuitous
  l <- length(coef(param(object, bootstrap = TRUE)))
  l1 <- length(coef(param(fit, bootstrap = TRUE)))

  # Re-fit the data after changing the data.frame. This could fail, since how
  # can you fit data with so few points? It doesn't make sense mathematicaly
  fit <- eval(object$call, sys.parent())

  #
  while (l > l1) {
    object$call$data <- data[sample(nrow(data), replace=TRUE),]
    fit <- eval(object$call, sys.parent())

    #
    l1 <- length(coef(param(fit, bootstrap = TRUE)))
  }

  # Invoke the bootstrap method
  boot(fit, bootstrap = TRUE)
}

