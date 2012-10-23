#' Return value for a zelig2-function
#'
#' This is an API-function that bridges a model-fitting function with a zelig
#' interface.
#' @note This is used internally by Zelig-dependent packages to instruct Zelig
#' how to evaluate the function call to a particular statistical model.
#' @param .function a function
#' @param ... a set of parameters to be evaluated symbolically
#' @return a ``z'' object which specifies how to evaluate the fitted model
#' @export
z <- function (.function, ...) {
  # Construct the function call
  .call <- as.call(as.list(match.call())[-1])
  .parent <- parent.frame()

  # Construct the object
  s <- list(
            call = .call,
            env = .parent
            )

  # Set attributes
  attr(s, 'baseenv') <- baseenv()
  attr(s, 'call') <- match.call()
  attr(s, 'function') <- substitute(.function)

  # Set the class
  class(s) <- 'z'

  # Return
  s
}
