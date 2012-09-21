#' Describe a ``exp'' model to Zelig
#' @usage \method{describe}{exp}(...)
#' @S3method describe exp
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.exp <- function(...) {
  list(
       authors = "",
       text = ""
       )
}
