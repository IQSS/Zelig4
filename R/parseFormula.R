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


  # Create the actual object
  res <- list(
              # Remember the source class
              source.class = class(obj),

              # Store the original copy of the formula
              formula = obj,

              # Store the terms
              terms   = terms(obj),

              # Use Zelig-style methods to get the responseTerms
              response  = getResponseTerms(obj),
              predictor = getPredictorTerms(list.formula),

              # Create the design matrix from the ``Formula'' package
              model.matrix = model.matrix(obj, data, lhs=NULL, rhs=NULL)
              )


  # Return
  class(res) <- "parseFormula"
  res
}
