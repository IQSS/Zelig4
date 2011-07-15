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
  messy.objects <- c("data.frame", "function")

  # Store values within 'messy.objects' within another environment, and give a 
  # pseudonym
  for (key in names(zelig2)) {
    obj <- zelig2[[key]]
    Class <- class(obj)

    if (any(Class %in% messy.objects) || is.object(obj)) {
      Name <- store.object(obj, envir, toupper(Class)[1])
      Call[[key]] <- as.name(Name)
    }
    else if (is.atomic(obj) && length(obj) > 5) {
      Name <- store.object(obj, envir, paste(toupper(Class)[1], length(obj), sep=""))
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
store.object <- function (obj, envir, name=NULL, prefix=".") {

  variables <- ls(envir=envir)
  
  # ensure name is unique
  while (name %in% variables) {
    name <- paste(prefix, name, sep="")
  }

  assign(name, obj, envir)

  name
}
