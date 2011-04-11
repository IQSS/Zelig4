#' Describe a `logit' model to Zelig
#'
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.logit <- function(...) {
  # parameters object
  parameters <- list(pi = list(
                       equations = c(1, 1),
                       tags.allowed = FALSE,
                       dep.var = TRUE,
                       exp.var = TRUE
                       )
                     )

  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2008,
       category = "dichotomous",
       package  = package.zelig("CORE", 1.0),
       parameters = parameters,
       text = "Logistic Regression for Dichotomous Dependent Variables"
       )
}
