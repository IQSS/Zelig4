#' Create Function Call
#'
#' 
#' @param Call a \code{call} object, typically specifying the original function
#'   call to \code{zelig}
#' @param zelig2 the return-value of the \code{zelig2} method
#' @param remove a list of character vectors specifying which parameters to
#'   ignore from the original call to \code{zelig}
#' @return a function call used to fit the statistical model
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig.call <- function(Call, zelig2, remove = NULL) {
  #
  envir <- new.env()

  # reserved words taken from the zelig2 method
  func <- as.name(zelig2$.function)
  hook <- zelig2$.hook

  # remove the reserved words
  zelig2$.function <- NULL
  zelig2$.hook <- NULL
  zelig2$.post <- NULL

  # make a list of the parameters to be passed to the external model
  args <- names(formals(as.character(func)))

  # remove certain parameters
  for (key in remove) {
    if (key %in% names(Call))
      Call[[key]] <- NULL
  }

  # remove invalid params
  for (key in names(Call[-1])) {
    if (! key %in% args)
      Call[[key]] <- NULL
  }

  # A static list of objects that do not printout well or should be stored
  # within a separate environment
  messy.objects <- c("data.frame", "function", 'matrix', "family", "function")
  neat.objects <- c("formula")

  # Store values within 'messy.objects' within another environment, and give a 
  # pseudonym
  for (key in names(zelig2)) {
    obj <- zelig2[[key]]
    Class <- class(obj)
    first.class <- Class[1]

    if (is.object(obj)) {
      if (all(Class %in% neat.objects))
        Call[[key]] <- obj
      else {
        Name <- store.object(obj, envir, ucfirst(first.class))
        Call[[key]] <- as.name(Name)
      }
    }
    else if (is.function(obj)) {
      Name <- store.object(obj, envir, "Function")
      Call[[key]] <- as.name(Name)
    }
    else if (is.atomic(obj) && length(obj) > 5) {
      Name <- store.object(obj, envir, paste(toupper(Class[1]), length(obj), sep=""))
      Call[[key]] <- as.name(Name)
    }
    else if (is.list(obj) && length(obj) > 5) {
      Name <- store.object(obj, envir, paste("List", length(obj), sep=""))
      Call[[key]] <- as.name(Name)
    }
    else {
      # this is a hack to prevent removal of elements if the value is NULL
      null.list <- list(NULL)
      names(null.list) <- key

      # the two statement are *slightly* different
      if (is.null(obj)) {
        Call <- as.call(append(as.list(Call), null.list))
      }
      else {
        Call[[key]] <- obj
      }
    }

  }

  # Change function value
  Call[[1]] <- func

  list(call=Call, envir=envir)
}

#' Store Object in Environment with a Fake Name
#'
#' This function takes the value of an object and stores it within a specified 
#' environment. This is similar to simply using the \code{assign} function, but
#' will not overwrite existing values in the specified environment. It
#' accomplishes this by appending a prefix to the name of the variable until
#' the name becomes unique.
#' @note This method does not correct invalid names. That is, there is no test
#'   to determine whether the submitted name is valid.
#' @param obj any object
#' @param envir an environment object, which will contain the object with the
#'   assigned name
#' @param name a character-string specifying the name that the object will be
#'   stored as in the specified environment
#' @param prefix a character string specifying the prefixes to append to names
#'   that already have matches in the destination environment
#' @return a character-string specifying the name of the object in the
#'   destination environment
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
store.object <- function (obj, envir, name=NULL, prefix=".") {

  variables <- ls(envir=envir)
  
  # ensure name is unique
  while (name %in% variables)
    name <- paste(prefix, name, sep="")

  assign(name, obj, envir)

  name
}

#' Uppercase First Letter of a String
#' 
#' This method sets the first character of a string to its uppercase,
#' sets all other characters to lowercase.
#' @param str a vector of charaqcter-strings
#' @return a vector of character strings
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
ucfirst <- function (str) {
  paste(
        toupper(substring(str, 1, 1)),
        tolower(substring(str, 2)),
        sep = ""
        )
}
