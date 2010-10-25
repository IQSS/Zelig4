# define generic function
ZeligCall <- function(...) UseMethod("ZeligCall")

# @model:   character-string or function for the model to call
# @default: list of values that are set statically (no user-input)
# @forward: list of values that the user must set at run-time
# @params:  list containing values that the user input
# return:   a zelig.call object which is later used to call the
#           foreign model
ZeligCall.default <- function(model, default=NULL, forward=NULL, params=NULL) {
  # error-catching
  if (!is.function(model) && !is.character(model))
    stop("`model` slot must be a function")

  if (any(names(default) == ""))
    stop("`default` cannot contain entires without a key")

  if (is.list(forward))
    forward <- unlist(forward)

  if (!(is.character(forward) || is.null(forward)))
    stop("`forward` slot must be a character-vector")

  # filter out things that we do not have values for
  forward <- Filter(function(x) x%in%names(params), forward)

  # create list of items to pass to function call
  params <- c(default, params[forward])

  # build list
  zc <- list(model = model, parameters = params)

  # assign class, and return
  class(zc) <- "ZeligCall"
  zc
}

# define stupid case
ZeligCall.ZeligCall <- function(zc) zc

# 
ZeligCall.list <- function(zc, ..., params) {
  if (is.name(zc[[1]]))
    zc[[1]] <- as.character(zc[[1]])

  if (!is.function(zc[[1]]) && !is.character(zc[[1]]))
    stop("list's first slot must contain a function")

  if (length(list(...)))
    stop()

  # split into entries with keys, and entries without keys
  split <- split.up(zc[-1])

  # create zelig.call, and return
  ZeligCall(zc[[1]],
            default=split$wordful,
            split$wordless,
            params
            )
}
