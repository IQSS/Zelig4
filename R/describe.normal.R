#' Describe the `normal' model to Zelig
#'
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.normal <- function(...) {
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
       category = "continuous",
       package  = package.zelig("CORE", 1.0),
       parameters = parameters,
       text = "Normal Regression for Continuous Dependent Variables"
       )
}
