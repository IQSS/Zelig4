#' Get Predictor Terms from Zelig-style Formulae
#'
#' This function extracts the predictor terms from a Zelig-style object.
#' @note This function is used exclusively in the development of Zelig-core.
#' @param x a Zelig-style formula ('formula' or 'list')
#' @param ... ignored parameters
#' @return a character-vector or NA
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
getPredictorTerms <- function (x, ...) {
  # The following functions are unsafe for general input, so they are being
  # kept as nested functions.

  # Extract "predictor" terms from a formula
  # @param x a formula
  # @param ... ignored parameters
  # @return a character-vector specifying the 
  # @author Matt Owen
  extractFromFormula <- function (form, ...) {
    TERMS <- terms(form)
    attr(TERMS, "term.labels")
  }

  # Extract "predictor" terms from a list of formulae
  # @param x a list
  # @param ... ignored parameters
  # @return a character-vector specifying the 
  # @author Matt Owen
  extractFromList <- function (x, ...) {
    as.vector(unlist(Map(extractFromFormula, x)))
  }

  # Beginning of work for function
  if (is.list(x))
    extractFromList(x)

  else if ("formula" %in% class(x))
    extractFromFormula(x)

  else {
    warning("The model formula must either ",
            "be a list of formula to work properly")
    NA
  }
}
