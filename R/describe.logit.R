#' Describe a `logit' model to Zelig
#' @S3method describe logit
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
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
       parameters = parameters,
       text = "Logistic Regression for Dichotomous Dependent Variables"
       )
}
