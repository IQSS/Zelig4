#' Parse Zelig-style Formulae
#'
#' Zelig uses three distinct types of formulae. This method is a re-design
#' of the Zelig function \code{parse.formula}.
#' @param obj a list or formula
#' @param data the data set associated with the formula object
#' @return an object of type "parseFormula". This object has slots specifying:
#' \item{terms}
#' \item{depVars}
#' \item{...}
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
parseFormula <- function (obj, data=NULL) {
  UseMethod("parseFormula")
}



# Parse Standard Formulae
# @param obj a formula
# @param data a data frame
# @return an object of type "parseFormula"
# @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @S3method parseFormula formula
parseFormula.formula <- function (obj, data=NULL) {

  1

  # Extract terms
  TERMS <- terms(formula)

  # Build the object
  res <- list(
              terms = TERMS,
              rawr = 1
              )

  # Return
  class(res) <- "parseFormula"
  res
}



# Parse List-Style Zelig Formulae
# @param obj a list of formulae
# @param data a data frame
# @return an object of type "parseFormula"
# @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @S3method parseFormula formula
parseFormula.list <- function (obj, data=NULL) {

  # Build the object
  res <- list(
              terms = TERMS,
              rawr = 1
              )

  # Return
  class(res) <- "parseFormula"
  res
}


