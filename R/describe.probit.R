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
