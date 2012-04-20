#' Attach Variables to a Function
#'
#' Returns a function, specified by the user, with the variables of a specified
#' environment attached. This, in essence, allows programmers to write functions
#' that have forms of private memory. This makes the function behave similarly
#' to an object.
#' 
#' @note This function is used by Zelig to ensure that particular method calls -
#' param, qi, bootstap - will contain the private variables: ``.fitted'',
#' ``.model'', ``.call'' and ``.env'' which respectively contain the fitted
#' model object, the name of the zelig model being invoked, the original call
#' to the model-fitting function and the environment in which to call the
#' function call.
#'
#' @param f a function which will be modified
#' @param env an environment variable which will be attached to the function
#' being returned
#' @param ... arbitrary key-value paired parameters which will be assigned to
#' the environment of the function being returned
#' @return the original function ``f'' with a different environment attached to
#' it.
#'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
attach.env <- function (f, env = NULL, ...) {

  # Ensure that a valid environment is passed in
  if (is.null(env))
    env <- new.env()


  # Expand dot parameters
  dots <- list(...)

  # Ensure that "env" is a valid environment
  if (is.null(env))
    env <- new.env()

  else if (!inherits(env, "environment")) {
    warning('Environment "env" is not a valid environment variable. ',
            'A default environment will be applied to "f" instead.')
    env <- new.env()
  }

  if (length(dots)) {
    # Add variables to the newly created environment
    for (key in names(dots))
      assign(key, dots[[key]], env)
  }

  # Modify the default environment of the function
  environment(f) <- env

  # Return the modified function
  f
}
