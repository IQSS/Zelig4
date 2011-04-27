#' Construct an Object to Describe Package Name and Version
#' @note This function is deprecated.
#' @param name a character-string specifying the name of a R-package
#' @param version a character-string specifying the version number of the
#'   R-package
#' @return a list containing 'name' and 'version' keys
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
package.zelig <- function(name, version="1.0") {

  if (!is.character(name) ||  length(name) > 1)
    stop()

  if (!(is.numeric(version) || is.character(version)) ||
      length(version) > 1)
    stop()

  self <- list(name = as.character(name),
               version = as.character(version)
               )
  self
}
