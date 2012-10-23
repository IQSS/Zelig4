#' Interface between probit model and Zelig
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param weights a numeric vector
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2probit <- function(formula, weights=NULL, ..., data)
  z(
    glm,
    # .hook = "robust.glm.hook",
    formula = formula,
    weights = weights,
    family  = binomial(link="probit"),
    model   = F,
    data    = data
    )
#' Param Method for the 'probit' Zelig Model
#' @note This method is used by the 'probit' Zelig model
#' @usage \method{param}{probit}(obj, num=1000, ...)
#' @S3method param negbinom
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.probit <- function(obj, num=1000, ...) {
  list(
       simulations = mvrnorm(n=num, mu=coef(.fitted), Sigma=vcov(.fitted)),
       alpha = NULL,
       fam = binomial(link="probit")
       )
}
#' Compute quantities of interest for 'probit' Zelig models
#' @usage \method{qi}{probit}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi probit
#' @param obj a 'zelig' object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#'   though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#'   interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.probit <- qi.logit
#' Describe the `probit' model to Zelig
#' @usage \method{describe}{probit}(...)
#' @S3method describe poisson
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.probit <- function(...){
  parameters <-list(mu = list(
                      equations = c(1,1),
                      tags.allowed = FALSE,
                      dep.vars = TRUE,
                      exp.vars = TRUE
                      )
                    )
  
  # return
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "dichotomous",
       parameters = parameters,
       text = "Probit Regression for Dichotomous Dependent Variables"
       )
}
