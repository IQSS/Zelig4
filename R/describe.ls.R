#' Describe a `ls' model to Zelig
#' `ls' stands for `least squares fit'
#'
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.ls <- function(...){
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
       category = "continuous",
       package  = package.zelig("stats", .1),
       parameters = parameters,
       text = "Least Squares Regression for Continuous Dependent Variables"
       )
}
