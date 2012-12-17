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
#' Param Method for the \code{tobit} Zelig Model
#' @note This method is used by the \code{tobit} Zelig model
#' @usage \method{param}{tobit}(obj, num, ...)
#' @S3method param tobit
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.tobit <- function(obj, num=1000, ...) {
  cov <- vcov(.fitted)
  mu <- c(coef(.fitted), log(.fitted$scale))

  # Return
  list(
       coef = mvrnorm(num, mu=mu, Sigma=cov),
       linkinv = NULL
       )
}
#' Compute quantities of interest for 'tobit' Zelig models
#' @usage \method{qi}{tobit}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi tobit
#' @param obj a 'zelig' object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#' though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#' interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.tobit <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  # This needs to be fixed.
  ev1 <- ev2 <- pr1 <- pr2 <- fd <- NA

  # return
  list("Expected Values: E(Y|X)"  = ev1,
       "Expected Values: E(Y|X1)" = ev2,
       "Predicted Values: Y|X"    = pr1,
       "Predicted Values: Y|X1"   = pr2,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}
#' Describe a ``tobit'' model to Zelig
#' @usage \method{describe}{tobit}(...)
#' @S3method describe tobit
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.tobit <- function(...) {
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2011,
       category = "continuous",
       text = "Linear regression for Left-Censored Dependent Variable"
       )
}
