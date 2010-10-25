# @simulations: vector or matrix containing simulated values
# @alpha: ancillary parameters
parameters <- function(simulations, alpha,
                       fam=NULL,
                       link=NULL,
                       linkinv=NULL)
{
  # 
  if (!missing(fam) && inherits(fam, "family")) {
    link <- fam$linkfun
    linkinv <- fam$linkinv
  }
  else if (missing(link)) {
    #warning("no link function")
  }

  else if (missing(linkinv)) {
    #warning("no inverse link function")
    linkinv <- .NumInverse(link)
  }

  # construct object
  p <- list(coefficients = simulations,
            alpha = alpha,
            link = link,
            linkinv = linkinv
            )

  # cast, and return
  class(p) <- "parameters"
  p  
}


# @param: parameters object
# return: 
alpha <- function(param)
  param$alpha


# @param: parameters object
# return: simulations
coef.parameters <- function(param) {
  param$coef
}
  

simulations.parameters <- function(param)
  param$coefficients


# @param: parameters object
# return: link function
link <- function(param)
  param$link


# @param: parameters object
# return: inverse function, or numerical approximation
linkinv <- function(param) {
  if (is.null(param$linkinv))
    .NumInverse(param$link)
  else
    param$linkinv
}
