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


  if (is.list(formula)) {
    m <- NULL

    for (form in formula) {
      m <- cbind(m, model.matrix(form, data))
    }

    t(as.matrix(m[, unique(colnames(m))]))
  }

  else {
    print(formula)
    print(class(data))
    print(model.matrix(formula, data))
    q()
    return(model.matrix(formula, data))
  }
}
