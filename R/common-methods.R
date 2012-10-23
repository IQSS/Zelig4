# This file is a quick-hack to fix a mistake placed in Zelig Core on Oct. 1st.
# The issue in Zelig should be fixed by November `12. :(

#' @S3method coef zelig
coef.zelig <- function (object, ...)
  coef(object$result, ...)

#' @S3method logLik zelig
logLik.zelig <- function (object, ...)
  logLik(object$result, ...)

#' @S3method plot zelig
plot.zelig <- function (x, ...)
  plot(x$result, ...)

#' @S3method vcov zelig
vcov.zelig <- function (object, ...)
  vcov(object$result, ...)
