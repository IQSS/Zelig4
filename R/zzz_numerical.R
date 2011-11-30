# numerical_functions.R
# ---------------------
# contents:
#  * .nderiv
#  * .nr
#  * .NumInverse


# Numerical Derivative
#
# This method computes the numerical derivative at a point
# @param f function (differentiable)
# @param stencil number of points in stencil. This is currently ignored.
# @param h size of mesh
# @return anonymous function with the approximation
# @note single variable numerical derivative
.nderiv <- function(f, stencil=5, h=sqrt(.Machine$double.eps)) {
  # return approximated derivative function
  function (x) {
    # construct the 5-point mesh, middle point omitted
    # since it gets deleted anyway
    x.stencil <- rep(x, 4) + c(2, 1, -1, -2)*h

    # compute approximation
    sum(sapply(x.stencil, f) %*% c(-1, 8, -8, 1))/12/h
  }
}



# @F: function to invert
# @f: derivative of function, or NULL to use numerical approximation
# @x: initial guess
# @tol: error-tolerance
# @h: mesh size
# @max.iter: number of iterations to perform before giving up
# return: df(x_0)/dx
# **note: newton-rhapson for single variables
# **suggestion: replace with C code, otherwise won't be truly fast-enough
.nr <- function(F, f=NULL, x = 1, a = 0,
                tol      = sqrt(.Machine$double.eps),
                h        = sqrt(.Machine$double.eps),
                max.iter = 50) {
  # save function to prevent recursions
  saved.function <- F

  # rewrite function to solve for a
  if (!missing(a))
    F <- function(x) saved.function(x) - a
  
  # if NULL assign numerical derivative
  if (is.null(f))
    f <- .nderiv(F)

  #
  count <- 1

  #
  while (abs(F(x)) > tol && count <= max.iter) {
    # increment counter
    count <- count + 1

    # if derivative is zero, or near it
    # (otherwise we have issues with solutions where x=0)
    if (abs(f(x)) < 10^-8) {
      x <- x + runif(1, min=-1, max=1)
      next
    }

    # iterate
    x <- x - F(x)/f(x)
  }

  if (count > max.iter)
    warning("approximation failed to converge given specified tolerance")

  # return result
  x
}


# @F:
# @f:
# @x: initial guess
# @tol: 
# return: a functional form of the newton-rhapson approximation
.NumInverse <- function(F, f=NULL, x = 1,
                        tol      = (.Machine$double.eps)^.5,
                        h        = sqrt(.Machine$double.eps),
                        max.iter = 50) {
  function (a) {
    res <- c()

    # kludgey, but just a hold-over for now
    for (val in a) {
      val <- .nr(F=F, f=f, x=x, a=val, tol=tol, h=h, max.iter=max.iter)
      res <- c(res, val)
    }

    res
  }
}
