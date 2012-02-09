#' Parse Zelig-style Formulae
#'
#' Zelig uses three distinct types of formulae. This method is a re-design
#' of the Zelig function \code{parse.formula}.
#' @param obj a list or formula
#' @param data the data set associated with the formula object
#' @return an object of type "parseFormula". This object has slots specifying:
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
parseFormula <- function (obj, data=NULL) {
  UseMethod("parseFormula")
}



#' Parse Standard Formulae
#'
#' This method parses a formula-style Zelig formula
#' @usage \method{parseFormula}{formula}(obj, data=NULL)
#' @param obj a formula
#' @param data a data frame
#' @return an object of type "parseFormula"
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @S3method parseFormula formula
parseFormula.formula <- function (obj, data=NULL) {

  # Extract terms
  TERMS <- terms(obj)

  #
  MODEL.MATRIX <- tryCatch(model.matrix(obj, data), error = function (e) NULL)

  # Build the object
  res <- list(
              formula = obj,
              terms = TERMS,
              response = getResponseTerms(obj),
              predictor = getPredictorTerms(obj),
              model.matrix = MODEL.MATRIX
              )

  # Return
  class(res) <- "parseFormula"
  res
}



#' Parse List-Style Zelig Formulae
#'
#' This method parses a list-style Zelig formula.
#' @usage \method{parseFormula}{list}(obj, data=NULL)
#' @param obj a list of formulae
#' @param data a data frame
#' @return an object of type "parseFormula"
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @S3method parseFormula list
parseFormula.list <- function (obj, data=NULL) {

  # Extract terms (and place in a list)
  TERMS <- Map(terms, obj)

  # 
  MODEL.MATRIX <- makeModelMatrix(obj, data)

  # Build the object
  res <- list(
              formula = obj,
              terms = TERMS,
              response = getResponseTerms(obj),
              predictor = getPredictorTerms(obj),
              model.matrix = MODEL.MATRIX
              )

  # Return
  class(res) <- "parseFormula"
  res
}



#' Parse ``Formula''-style Zelig Formulae
#'
#' This method parses a ``Formula''-style Zelig formula. This is to support the
#' ``Formula'' object. It seems like it has the right idea when it comes to 
#' expressing multiple responses.
#' @usage \method{parseFormula}{Formula}(obj, data=NULL)
#' @param obj a list of formulae
#' @param data a data frame
#' @return an object of type ``parseFormula''
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @S3method parseFormula Formula
parseFormula.Formula <- function (obj, data=NULL) {

  # Create and empty list
  list.formula <- list()

  # This loop goes through all the list response and predictor terms and 
  # creates a "Zelig-style" list based on it. This is so we can extract response
  # and predictor terms with "getResponstTerms" and "getPredictorTerms" in a
  # manageable way!
  #
  # Future implementations my move this loop's functionality into the methods:
  #   getResponseTerms.Formula
  #   getPredictorTerms.Formula
  #
  # Right now, this seems right though, considering it's a one-off (for now)
  for (resp in attr(obj, "lhs")) {
    # Iterate through all response variables

    for (pred in attr(obj, "rhs")) {
      # Iterate through all predictor variables

      # Append response variable and predictor terms
      # "ccc" is probably going to be convention for a call object in Zelig
      # models since "CALL", "call", "Call" all seem too similar to "call".
      # And we need to break
      ccc <- call("~", resp, pred)

      # Cast from a "call" object to a "formula" object
      ccc <- as.formula(ccc)

      # Append to list
      list.formula <- append(list.formula, ccc)

    }
  }

  # Create the actual object
  res <- list(
              formula = obj,
              terms   = terms(obj),
              response  = getResponseTerms(list.formula),
              predictor = getPredictorTerms(list.formula),
              model.matrix = model.matrix(obj, data, lhs=NULL, rhs=NULL)
              )


  # Return
  class(res) <- "parseFormula"
  res
}
