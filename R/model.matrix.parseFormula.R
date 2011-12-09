#' Construct Design Matrix from a Parsed, Zelig-style Formula
#'
#' This method constructs a design matrix from a Zelig-style formula. This
#' matrix is commonly used in statistical simulation, and will likely be
#' relevent as the relevant form of a \code{setx} object.
#' @usage \method{model.matrix}{parseFormula}(object, data = NULL, ...)
#' @note This method is primarily used by the \code{setx} function.
#' @param object a "parseFormula" object
#' @param data a "data.frame"
#' @param ... ignored parameters
#' @return a "model.matrix" specifying information relevant to a statistical
#' model
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @S3method model.matrix parseFormula
model.matrix.parseFormula <- function (object, data = NULL, ...) {

  if (is.null(object$model.matrix))
    # Note that if data is NULL, then "makeModelMatrix" will return NULL
    makeModelMatrix(formula(object), data)

  else if (!missing(data))
    # If data is supplied, recompute the model matrix
    makeModelMatrix(formula(object), data)

  else
    # Otherwise use the previous stored value (which still might be NULL)
    object$model.matrix

}
