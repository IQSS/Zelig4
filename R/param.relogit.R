#' Estimate Parameters for the ``relogit'' Zelig Mdoel
#'
#' Returns estimates on parameters, as well as, specifying link and
#' inverse-link functions.
#' @note This method merely calls ``param.logit''.
#' @usage \method{param}{relogit}(obj, num, ...)
#' @S3method param relogit
#' @param obj a zelig object containing the fitted model
#' @param num an integer specifying the number of simulations to compute
#' @param ... unspecified parameters
#' @return a list specifying important parameters for the ``relogit'' model
param.relogit <- function (obj, num, ...)
  param.logit(obj, num, ...)


#' Estimate Parameters for the ``relogit'' Zelig Mdoel
#'
#' Returns estimates on parameters, as well as, specifying link and inverse-link
#' functions.
#' @usage \method{param}{relogit2}(obj, num, x, ...)
#' @S3method param relogit2
#' @param obj a zelig object containing the fitted model
#' @param num an integer specifying the number of simulations to compute
#' @param x ideally we should be able to remove this parameter
#' @param ... unspecified parameters
#' @return a list specifying important parameters for the ``relogit'' model
param.relogit2 <- function (obj, num, x, ...) {
  stop("Currently zelig does not support relogit models containing 2 ",
       "tau parameters")

  pping <- function(tmp0, tmp1, num, bootstrap, x) {

    par0 <- param.relogit(tmp0, num=num, x=x, bootstrap=bootstrap)
    par1 <- param.relogit(tmp1, num=num, x=x, bootstrap=bootstrap)

    P00 <- qi.relogit(tmp0, par0, x=x)

    P00 <- as.matrix(qi.relogit(tmp0, param = par0, x=x)$qi$ev)
    message("P01")
    P10 <- as.matrix(qi.relogit(tmp1, param = par1, x=x)$qi$ev)

    test <- P00[,1] < P10[,1]
    par0 <- as.matrix(par0[test,])
    par1 <- as.matrix(par1[test,])
    list(par0 = par0, par1 = par1)
  }
  tmp0 <- tmp1 <- object

  tmp0$result <- object$result$lower.estimate
  tmp1$result <- object$result$upper.estimate

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
