#' Interface between the Zelig Model tobit and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param below a numeric or infinite specifying a lower boundary for censored
#' responses
#' @param above a numeric or infinite specifying an upper boundary for censored
#' responses
#' @param robust a boolean specifying whether to produce robust error estimates
#' @param cluster ...
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2tobit <- function (
                         formula, ..., 
                         below = 0, above = Inf, 
                         robust = FALSE,
                         cluster = NULL,
                         data
                         ) {

  if (!(is.null(cluster) || robust))
    stop("If cluster is specified, then `robust` must be TRUE")

  # Add cluster term
  if (robust || !is.null(cluster))
    formula <- cluster.formula(formula, cluster)

  # Make surv demands that the model 
  formula <- make.surv(formula, below, above)
  formula <- cluster.formula(formula, cluster)

  list(
       .function = "survreg",

       formula = formula,
       dist = "gaussian",
       data = data,
       robust = robust,
       ...
       )
}


#
make.surv <- function (formula, below, above) {

  lhs <- formula[[2]]

  if (grepl("Surv", as.character(lhs)))
    return(formula)

  if (!(is.numeric(below) && is.numeric(above))) {
    warning("`below` and `above` must be numeric; ",
            "returning the original formula")
    return(formula)
  }

  if (above == Inf) {
    # Empty?
    # This seems like a mistake inherited from old code
  }

  else if (below == -Inf && above == Inf)
    stop("This model does not support censoring. Try the \"normal\" model")

  else if (below == -Inf && above != Inf)
    stop("This model does not support right-censored data")

  else if (is.finite(below) && is.finite(above))
    stop("This model does not support interval-censored data")

  # That is, this model only supports left-censored data
  # Surv( <outcome> , <below> < <outcomes> )
  lhs <- call("Surv", lhs, call("<", below, lhs), type="left")

  # Place back within formula
  formula[[2]] <- lhs

  # Return
  formula
}
