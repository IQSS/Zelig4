#' Estimate Parameters for the ``relogit'' Zelig Mdoel
#'
#' Returns estimates on parameters, as well as, specifying link and
#' inverse-link functions.
#' @S3method param relogit
#' @param object a zelig object containing the fitted model
#' @param num an integer specifying the number of simulations to compute
#' @param x ideally we should be able to remove this parameter
#' @return ...
param.relogit <- param.logit

#' Estimate Parameters for the ``relogit'' Zelig Mdoel
#'
#' Returns estimates on parameters, as well as, specifying link and
#' inverse-link functions.
#' @S3method param relogit2
#' @param object a zelig object containing the fitted model
#' @param num an integer specifying the number of simulations to compute
#' @param x ideally we should be able to remove this parameter
#' @return ...
param.relogit2 <- function (object, num, x, ...) {

  names(object$result)

  message("param.relogit2")
  message("param.relogit2")
  message("param.relogit2")
  message("param.relogit2")

  list(
       coef = mvrnorm(num, mu = coef(object), Sigma = vcov(object)),
       alpha = NULL,
       fam = binomial(link = "logit")
       )
}
