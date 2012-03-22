#' simulate quantities of interest for the zelig ``relogit'' model
#'
#' @s3method qi relogit
#' @param obj a zelig object, containing the fitted ``relogit'' model
#' @param x a ``setx'' object
#' @param x1 a ``setx'' object
#' @param num an integer specifying the number of simulations to compute
#' @param param a ``parameter'' obejct containing information about the link,
#' inverse-link, and simulated parameters
#' @return a param
qi.relogit <- qi.logit


#' simulate quantities of interest for the zelig ``relogit'' model
#'
#' @param obj a zelig object, containing the fitted ``relogit'' model
#' @param x a ``setx'' object
#' @param x1 a ``setx'' object
#' @param num an integer specifying the number of simulations to compute
#' @param param a ``parameter'' obejct containing information about the link,
#' inverse-link, and simulated parameters
#' @return a param
qi.relogit2 <- function (object, simpar, x, x1 = NULL, y = NULL) {
  num <- nrow(simpar$par0)
  tmp0 <- object$lower.estimate
  tmp1 <- object$upper.estimate
  
  low <- qi.relogit(tmp0, simpar$par0, x, x1)
  up <- qi.relogit(tmp1, simpar$par1, x, x1)
  PP <- PR <- array(NA, dim = c(num, 2, nrow(x)),
                    dimnames = list(NULL, c("Lower Bound", "Upper Bound"),
                      rownames(x)))
  PP[,1,] <- P00 <- low$qi$ev
  PP[,2,] <- P10 <- up$qi$ev
  qi <- list(ev = PP)
  qi.name <- list(ev = "Expected Values: E(Y|X)")
  if (!is.null(x1)) {
    FD <- RR <- array(NA, dim = c(num, 2, nrow(x)),
                      dimnames = list(NULL,
                                      d2 = c("Lower Bound", "Upper Bound"), 
                                      rownames(x)
                      ))

    sim01 <- qi.glm(tmp0, simpar$par0, x = x1, x1 = NULL)
    sim11 <- qi.glm(tmp1, simpar$par1, x = x1, x1 = NULL)
    tau0 <- object$lower.estimate$tau
    tau1 <- object$upper.estimate$tau
    P01 <- as.matrix(sim01$qi$ev)
    P11 <- as.matrix(sim11$qi$ev)
    OR <- (P10/(1-P10)) / (P00/(1-P00))
    RR[,1,] <- pmin(as.matrix(P01/P00), as.matrix(P11/P10))
    RR[,2,] <- pmax(as.matrix(P01/P00), as.matrix(P11/P10))
    RD0 <- as.matrix(P01-P00)
    RD1 <- as.matrix(P11-P10)
    RD <- as.matrix((sqrt(OR)-1) / (sqrt(OR)+1))
    ## checking monotonicity
    y.bar <- mean(object$y)
    beta0.e <- coef(tmp0)
    beta1.e <- coef(tmp1)
    ## evaluating RD at tau0 and tau1
    RD0.p <- 1/(1+exp(-t(beta0.e) %*% t(x1))) - 1/(1+exp(-t(beta0.e) %*% t(x)))
    RD1.p <- 1/(1+exp(-t(beta1.e) %*% t(x1))) - 1/(1+exp(-t(beta1.e) %*% t(x)))
    ## evaluating RD at tau0+e and tau1+e
    e <- 0.001
    beta0.e["(Intercept)"] <- beta0.e["(Intercept)"]+log(1-tau0)-log(tau0) -
      log(1-tau0-0.001)+log(tau0+0.001)
    beta1.e["(Intercept)"] <- beta1.e["(Intercept)"]+log(1-tau1)-log(tau1) -
      log(1-tau1-e)+log(tau1+e)
    RD0.e <- 1/(1+exp(-t(beta0.e) %*% t(x1))) - 1/(1+exp(-t(beta0.e) %*% t(x)))
    RD1.e <- 1/(1+exp(-t(beta1.e) %*% t(x1))) - 1/(1+exp(-t(beta1.e) %*% t(x)))
    ## checking the sign and computing the bounds
    check <- sum((RD1.e-RD1.p) * (RD0.e-RD0.p))
    if (check > 0) {
      FD[,1,] <- pmin(RD0, RD1)
      FD[,2,] <- pmax(RD0, RD1)
    }
    else {
      FD[,1,] <- pmin(RD0, RD1, RD)
      FD[,2,] <- pmax(RD0, RD1, RD)
    }
    qi$fd <- FD
    qi$rr <- RR
    qi.name$fd <- "First Differences: P(Y=1|X1) - P(Y=1|X)"
    qi.name$rr <- "Risk Ratios: P(Y=1|X1) / P(Y=1|X)"
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, num), nrow = num, byrow = TRUE)
#      tmp.ev <- qi$tt.ev <- yvar - qi$ev
#      tmp.pr <- qi$tt.pr <- yvar - as.integer(qi$pr)
#      qi.name$tt.ev <- "Unit Treatment Effect for the Treated: Y - EV"
#      qi.name$tt.pr <- "Unit Treatment Effect for the Treated: Y - PR"
    tmp.ev <- yvar - qi$ev
    tmp.pr <- yvar - as.integer(qi$pr)
    qi$att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = num)
    qi$att.pr <- matrix(apply(tmp.pr, 1, mean), nrow = num)
    qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
    qi.name$att.pr <- "Average Treatment Effect for the Treated: Y - PR"
  }
  return(list(qi = qi, qi.name = qi.name))
}
