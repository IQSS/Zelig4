#' Constructor for the 'ignore' class
#' This class is included for future use, and is currently
#' not used in any Zelig model. It is designed for use with
#' zelig2* functions
#' @param default default value
#' @param type ignored parameter
#' @return an 'ignore' object
#' @export
#' @author Matt Owen \emph{mowen@@iq.harvard.edu}
ignore <- function (default = NULL, type = "no pass") {

  self <- default
  class(self) <- "ignore"

  # store information, set class, and return
  self <- list(
               default = default,
               type    = type
               )
  class(self) <- "ignore"
  self
}
