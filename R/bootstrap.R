#' Generic Method for ``bootstrap''
#'
#' This mehtod is intended to be overried by statistical models that would like
#' to support statistical bootstrapping.
#' @param obj a ``zelig'' object that will be used to produce boot-strapped
#' parameters
#' @param num an integer specifying the number of simulations to produce
#' @param ... extra parameters to be passed to the ``boot'' method. These are
#' typically ignored, but is included for further expansion.
#' @return a list containing information concerning link, link-inverses, etc.
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
bootstrap <- function (obj, ...) {
  UseMethod("bootstrap")
}

#' Produce Boot-strapped Parameters for a Statistical Model
#'
#' ...
#' @param obj 1
#' @param num an integer specifying the number of simulations to produce
#' @param ...
#' @return a list with the ``link'', ``linkinv'' and ``family'' slots set
#' @S3method bootstrap default
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
bootstrap.default <- function (obj, num, ...) {

  list(
       beta = coef(obj)
       )
}
