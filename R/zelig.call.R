#' Construct a Call to a Model Fitting Function
#' This function parses the parameters submitted by the 'zelig2' function and
#' constructs an object, useable by Zelig, to fit the model.
#' @note This function is exclusively used internally by Zelig.
#' @param model character-string or function for the model to call
#' @param params list containing values that the user input
#' @param from.call the original Zelig call
#' @return a zelig.call object which is later used to call the model fitting
#'   function
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig.call <- function(model, params=NULL, from.call=NULL) {
  # error-catching
  if (!is.function(model) && !is.character(model))
    stop("'model' slot must be a function")


  # hide away params in a distant environment
  # this:
  #  * this makes the function signature look pertty)
  #  * ensure we can refer to symbols correctly
  #  * allows user to ACTUALLY be able to recreate
  #    the function call to the foreign model
  # ".store.values" is in "zzz_environment_hacks.R"
  stored.values <- .store.values(params)


  # construct call
  call <- append(as.name(model), stored.values$params)
  call <- as.call(call)


  # build object
  zc <- list(
             model = model,
             parameters = stored.values$params,
             envir = stored.values$envir,
             call  = call,
             oldcall = from.call
             )

  # assign class, and return
  class(zc) <- "zelig.call"
  zc
}
