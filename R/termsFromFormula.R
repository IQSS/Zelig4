#' Extract Terms from Zelig-style Formulae
#'
#' This method is a sugary function to extract terms from any type of 
#' Zelig-style formula.
#' @param obj a Zelig-style formula
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
termsFromFormula <- function (obj) {
  # Do not put all of this function on one line, because it will make error
  # messages long and confusing
  obj <- parseFormula(obj)

  # Actually extract the terms, then return
  terms(obj)
}
