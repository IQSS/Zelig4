#' Describe the `poisson' model to Zelig
#' @S3method describe poisson
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.poisson <- function(...) {
  # parameters object
  parameters <- list(lambda = list(
                       equations = c(1, 1),
                       tags.allowed = FALSE,
                       dep.vars = TRUE,
                       exp.vars = TRUE
                       )
                     )

  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "count",
       package  = package.zelig("CORE", 1.0),
       parameters = parameters,
       text = "Poisson Regression for Event Count Dependent Variables"
       )
}
