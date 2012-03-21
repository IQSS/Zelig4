#' Hook for ``glm'' Models in Zelig
#'
#' Adds support for robust error-estimates in the Zelig ``glm'' models.
#' @param obj a zelig object
#' @param zcall the original call to the zelig model
#' @param call the call that will be evaluated for the 
#' @param robust a logical specifying whether or not to use robust error
#' estimates
#' @param ... ignored parameters
#' @return the fitted model object
#' @export
robust.glm.hook <- function (obj, zcall, call, robust = FALSE, ...) {

  # If "robust" is a list, 
  if (is.list(robust)) {

    # if none of the entries of robust belong to the vector below
    if (!any(rob$method %in% c("vcovHAC", "kernHAC", "weave")))
      stop("robust contains elements that are not supported.")

    # Acquire the value of the robust parameter
    obj$robust <- robust
  }
  else if (!is.logical(robust))
    stop("Invalid input for robust: choose either TRUE or a list of options.")

  # Set as a robust generalized linear model model (in addition to other types)
  class(obj) <- c("glm.robust", class(obj))

  # Return...
  obj
}
