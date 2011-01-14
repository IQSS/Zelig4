# @model:   character-string or function for the model to call
# @default: list of values that are set statically (no user-input)
# @forward: list of values that the user must set at run-time
# @params:  list containing values that the user input
# return:   a zelig.call object which is later used to call the
#           foreign model
zelig.call <- function(model, default=NULL, params=NULL) {
  # error-catching
  if (!is.function(model) && !is.character(model))
    stop("`model` slot must be a function")


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
             call  = call
             )

  # assign class, and return
  class(zc) <- "zelig.call"
  zc
}
