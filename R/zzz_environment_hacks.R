# @params: a list of parameters to create a Zelig Call
# return: a list containing:
#           * parameters - a list of symbols
#           * envir - an environment where variables
#             contained in envir exist
.store.values <- function(params) {
  #
  e <- new.env()

  #
  named.params <- list()
  names <- names(params)

  #
  for (key in names) {
    # prefix keyname if there is an overlap
    keyname <- .prefix(key, envir=e, prefix="stored")

    # store variable `keyname` in zelig environment
    assign(keyname, params[[key]], envir=e)

    # create a symbolic list
    named.params[[key]] <- as.name(keyname)
  }

  # return list
  list(params=named.params, envir=e)
}


# @mc: a function call
# return: an environment holding the evaluated expressions
#         from the function call
.store.call.envir <- function(mc, envir=new.env()) {
  lis <- as.list(mc)[-1]

  for (key in names(lis))
    assign(key, eval(lis[[key]], envir=parent.frame()), envir=envir)

  envir
}


hide <- function(...) {
  if (missing(..2))
    warning()

  args <- c(...)
  names <- names(args)

  if (length(names) != length(args))
    names <- c("object")

  ret <- ..1
  attr(ret, ".hide-display-name") <- ..2
  class(ret) <- c("hide", class(ret))
  ret
}

print.hide <- function(x, ...)
  print(attr(x, ".hide-display-name"))


# @name: a character-string specifying the name of a variable
# @envir: an environment variable to search
# @prefix: a character-string
# @sep: a character-string that 
.prefix <- function(name, envir, prefix="zelig", sep=".") {

  # check to make sure this is an environment variable
  if (!is.environment(envir)) {
    warning()
    envir <- globalenv()
  }

  # ensure some name is returned
  if (!is.character(c(name, prefix, sep))) {
    warning()
    name
  }

  else if (length(name) > 1 || length(prefix) > 1 || length(sep) > 1) {
    warning()
    name
  }

  else if (!nchar(name)) {
    warning()
    sep <- "."
  }

  else {
    while(exists(name, envir=envir))
      name <- paste(prefix, name, sep=sep)

    # return if nothing wonky happened
    name
  }
}
