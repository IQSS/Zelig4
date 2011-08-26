#' Compute quantities of interest for 'logit' Zelig models
#' @usage \method{qi}{logit}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi logit
#' @param obj a 'zelig' object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#'   though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#'   interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.logit <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {
  #
  ev1 <- .compute.ev(obj, x, num, param)
  pr1 <- matrix(nrow=nrow(ev1), ncol=ncol(ev1))

  
  # init
  ev2 <- .compute.ev(obj, x1, num, param)
  pr2 <- fd <- NA

  
  # fill?
  for (i in 1:ncol(ev1))
    pr1[,i] <- as.character(rbinom(length(ev1[,i]), 1, ev1[,i]))


  #
  if (!is.null(x1)) {
    pr2 <- matrix(nrow=nrow(ev2), ncol=ncol(ev2))

    for (i in 1:ncol(ev2))
      pr2[,i] <- as.character(rbinom(length(ev2[,i]), 1, ev2[,i]))

    fd <- ev2-ev1
  }


  # return
  list("Expected Values: E(Y|X)" = ev1,
       "Predicted Values: Y|X" = pr1,
       "Expected Values (for X1)" = ev2,
       "Predicted Values (for X1)" = pr2,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}

#' compute expected values
.compute.ev <- function(obj, x=NULL, num=1000, param=NULL) {
  if (is.null(x))
    return(NA)

  coef <- coef(param)
  link.inverse <- linkinv(param)

  eta <- coef %*% t(x)

  theta <- matrix(link.inverse(eta), nrow = nrow(coef))

  ev <- matrix(link.inverse(eta), ncol=ncol(theta))

  ev
}
