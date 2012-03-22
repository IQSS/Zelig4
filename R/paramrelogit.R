#' Estimate Parameters for the ``relogit'' Zelig Mdoel
#'
#' Returns estimates on parameters, as well as, specifying link and
#' inverse-link functions.
#' @S3method param relogit
#' @param object a zelig object containing the fitted model
#' @param num an integer specifying the number of simulations to compute
#' @param x ideally we should be able to remove this parameter
#' @return ...
param.relogit <- function (object, num, x, ...) {
  message("\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\")
  message("\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\")

  print(class(object))
  print(class(object$result))

  # If the result object is a "relogit2" object (meaning 2 values for tau were
  # passed at runtime, we use a custom param method
  if (inherits(object$result, "Relogit2"))
    param.relogit2(object, num, x, ...)

  # Otherwise, the standard logit method is sufficient
  else
    param.logit(object, num, x, ...)
}

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


  pping <- function(tmp0, tmp1, num, bootstrap, x) {
    par0 <- param.relogit(tmp0, num=num, x=x, bootstrap=bootstrap)
    par1 <- param.relogit(tmp1, num=num, x=x, bootstrap=bootstrap)
    P00 <- as.matrix(qi.relogit(tmp0, par0, x=x)$qi$ev)
    P10 <- as.matrix(qi.relogit(tmp1, par1, x=x)$qi$ev)
    test <- P00[,1] < P10[,1]
    par0 <- as.matrix(par0[test,])
    par1 <- as.matrix(par1[test,])
    list(par0 = par0, par1 = par1)
  }
  print(class(object$result$lower.estimate))
  q()
  tmp0 <- object$result$lower.estimate
  tmp1 <- object$result$upper.estimate
  tmp <- pping(tmp0, tmp1, num = num, bootstrap=bootstrap, x=x)
  par0 <- tmp$par0
  par1 <- tmp$par1
  while (nrow(par0) < num) {
    tmp <- pping(tmp0, tmp1, num=num, bootstrap=bootstrap, x=x)
    par0 <- rbind(par0, tmp$par0)
    par1 <- rbind(par1, tmp$par1)
  }
  if (nrow(par0) > num) {
    par0 <- par0[1:num,]
    par1 <- par1[1:num,]
  }
  par0 <- as.matrix(par0)
  par1 <- as.matrix(par1)
  rownames(par0) <- 1:nrow(par0)
  rownames(par1) <- 1:nrow(par1)
  return(list(par0 = par0, par1 = par1))    
}
