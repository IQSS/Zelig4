#' Make a Model Matrix from a Zelig-Style Formula
#' 
#' This is a helper function that creates a \code{model.matrix} like object
#' of Zelig-style formulae.
#' @param formula a Zelig-style formula
#' @param data a \code{data.frame}
#' @return a design (or model) matrix
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
makeModelMatrix <- function (formula, data) {

  if (missing(data) || is.null(data))
    return(NULL)

  fromList <- function (x, data) {
    m <- NULL

    for (formula in x)
      m <- cbind(m, model.matrix(formula, data))

    m[, unique(colnames(m))]
  }

  if (is.list(formula))
    fromList(formula, data)

  else
    model.matrix(formula, data)
}
