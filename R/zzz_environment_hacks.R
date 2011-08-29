#' Store Named Values in an Newly Created Environment
#' @note This function is exclusively used internally by Zelig
#' @param params a list of parameters to create a Zelig Call
#' @return a list containing:
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

    # the following if-statements are meant to prevent the storage of
    # small call-function readable data-types. That is, these things
    # are very easily printable and have no need to be tucked away
    if (inherits(params[[key]], 'formula')) {
      named.params[[key]] <- params[[key]]
      next
    }

    if (is.character(params[[key]])) {
      named.params[[key]] <- params[[key]]
      next
    }

    if (is.numeric(params[[key]])) {
      named.params[[key]] <- params[[key]]
      next
    }

    if (inherits(params[[key]], "literal")) {
      named.params[[key]] <- params[[key]]$value
      next
    }


    # Everything below here gets processed
    # in the default manner
    # .............
    
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


#' Append a Prefix to a Character String
#' @note This function is exclusively used internally by Zelig
#' @param name a character-string specifying the name of a variable
#' @param envir an environment variable to search
#' @param prefix a character-string to prefix the string with
#'   this is applied until the name is unique
#' @param sep a character-string that separates prefix and name
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


#' Attach Environment and Evaluate Expression
#' @param ..1 an expression to evaluate
#' @param ..2 the environment to evaluate the expression in
#' @return evaluation of ..1
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
eval.in <- .call <- function (...) {
  attach(..2)
  res <- suppressMessages((..1))
  detach(..2)
  res
}
