#' Class to Specify Optional Parameters
#' param default
#' param type 
#' value an "ignore" object
ignore <- function (default = NULL, type = "no pass") {

  self <- default
  class(self) <- "ignore"

  # assume 'no pass' if type is invalid
  if ( ! type %in% valid.ignore.types() ) {
    warning("Invalid type ", type, " assuming type='no pass'")
    return(self)
  }

  # store information, set class, and return
  self <- list(
               default = default,
               type    = type
               )
  class(self) <- "ignore"
  self
}

valid.ignore.types <- function() {
  c(
    "missing_or_null",
    "null",
    "default",
    "no pass"
    )
}
