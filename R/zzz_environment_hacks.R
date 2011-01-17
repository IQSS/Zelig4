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


# @name:   a character-string specifying the name of a variable
# @envir:  an environment variable to search
# @prefix: a character-string to prefix the string with
#          this is applied until the name is unique
# @sep:    a character-string that separates prefix and name
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


# @.zc: a ZeligCall object
# @.data: a data.frame
# return: the object ran in an insulated environment
.run <- function(...) {
  # attach to ensure all the variables are known
  attach(..1$envir)
  attach(..2)

  # run functin call
  .result <- do.call(as.character(..1$call[[1]]),
                     as.list(..1$call)[-1],
                     envir=..1$envir
                     )

  # detach relevant stuff
  detach(..1$envir)
  detach(..2)

  # return object
  .result
}
