#' Describe the \code{gamma} model to Zelig
#' @S3method describe default
#' @param ... ignored parameters
#' @return a list of important information
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.gamma <- function(...) {
  # parameters object
  parameters <- list(lambda = list(
                       equations = c(1, 1),
                       tags.allowed = FALSE,
                       dep.var = TRUE,
                       exp.var = TRUE
                       )
                     )

  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "bounded",
       parameters = parameters,
       text = "Gamma Regression for Continuous, Positive Dependent Variables"
       )
}
