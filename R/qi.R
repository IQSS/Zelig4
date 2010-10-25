qi <- function(z, x=NULL, x1=NULL, ...) {
  # error-catching
  if (!inherits(z, "zelig"))
    stop("z must be of type \"zelig\"")

  if (!(is.null(x) || inherits(x, "setx")))
    stop("x must be of type \"setx\"")

  if (!(is.null(x1) || inherits(x1, "setx")))
    stop("x1 must be of type \"setx\"")

  # then use the method
  UseMethod("qi")
}
