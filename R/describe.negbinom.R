#' Describe the \code{negbinom} model to Zelig
#' @note \code{negbinom} stands for "negative binomial"
#' @usage \method{describe}{negbinom}(...)
#' @S3method describe negbinom
#' @param ... ignored parameters
#' @return a list to be processed by \code{as.description}
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.negbinom <- function(...) {
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
       category = "count",
       parameters = parameters,
       text = "Negative Binomial Regression for Event Count Dependent Variables"
       )
}
