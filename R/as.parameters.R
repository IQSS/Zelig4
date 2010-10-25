# @...:
# **note: declares generic
as.parameters <- function(...) UseMethod("as.parameters")

# @params: a list object
# @num:    number of simulations
as.parameters.list <- function(params, num=NULL) {
  #
  coefficients <- if ("simulations" %in% names(params))
    params$simulations
  else if (num < length(params))
    params[1:num]
  else
    params[[1]]

  # 
  alpha <- if ("alpha" %in% names(params))
    params$alpha
  else if (num < length(params))
    tail(params, -num)


  # link function
  link <- if (!is.null(params$link))
    params$link

  # link-inverse function
  linkinv <- if (!is.null(params$linkinv))
    params$linkinv

  # family object, has both a link and link-inverse
  fam <- params$fam

  # return
  parameters(coefficients, alpha, fam=fam, link=link, linkinv=linkinv)
}


# @params: a parameters object
# return:  same parameter object
# **note:  this is an identity function
as.parameters.parameters <- function(params, ...)
  params


# @object: any non-supported data-type
# return:  the object passed in
# **note:  identity, but with a warning
as.parameters.default <- function(p, num=NULL) {
  if (!missing(num)) {
    alpha <- if (num < nrow(p))
      tail(p, -num)

    #
    parameters(simulations=head(p, num), alpha=alpha)
  }
  
  else
    parameters(simulations=p, alpha=NULL)
}

#
"[.parameters" <- function() {
}
