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
  # Get a random sample of the data set
  d <- data[i,]

  # Extract the call object
  .call <- object$call

  # Replace the data frame with an appropriate one
  .call$data <- d

  # Fit the model
  fit <- eval(.call, sys.parent())

  # Get the appropriate boot function


  #
  coef(fit)
}
