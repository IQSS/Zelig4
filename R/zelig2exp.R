#' Interface between the Zelig Model exp and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param robust a boolean specifying whether to use robust error estimates
#' @param cluster a vector describing the clustering of the data
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2exp <- function (formula, ..., robust = FALSE, cluster = NULL, data) {

  if (!(is.null(cluster) || robust))
    stop("If cluster is specified, then `robust` must be TRUE")

  # Add cluster term
  if (robust || !is.null(cluster))
    formula <- cluster.formula(formula, cluster)

  # Return
  list(
       .function = "survreg",

       formula = formula,
       dist = "exponential",
       robust = robust,
       data = data,
       ...
       )
}


stratify.rqs <- function (obj) {
  x <- vector("list", length(obj$tau))

  for(i in 1:length(obj$tau)) {
    xi <- obj

    xi$coefficients <- xi$coefficients[, i]
    xi$residuals <- xi$residuals[, i]
    xi$tau <- xi$tau[i]
    class(xi) <- "rq"

    x[[i]] <- xi 
  }

  names(x) <- obj$tau
  x
}
