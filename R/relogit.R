#' Fit a rare-event logistic model in Zelig
#' 
#' Fits a rare-event (``relogit'') model.
#' @param formula a formula object
#' @param data ...
#' @param tau ...
#' @param bias.correct ...
#' @param case.control ...
#' @param ... ???
#' @return a ``relogit'' ``glm'' object
#' @export
relogit <- function(
                    formula,
                    data = sys.parent(),
                    tau = NULL,
                    bias.correct = TRUE,
                    case.control = "prior",
                    ...
                    ){
  mf <- match.call()
  mf$tau <- mf$bias.correct <- mf$case.control <- NULL
  if (!is.null(tau)) {
    tau <- unique(tau)
    if (length(case.control) > 1)
      stop("You can only choose one option for case control correction.")
    ck1 <- grep("p", case.control)
    ck2 <- grep("w", case.control)
    if (length(ck1) == 0 & length(ck2) == 0)
      stop("choose either case.control = \"prior\" ",
           "or case.control = \"weighting\"")
    if (length(ck2) == 0)
      weighting <- FALSE
    else 
      weighting <- TRUE
  }
  else
    weighting <- FALSE
  if (length(tau) > 2)
    stop("tau must be a vector of length less than or equal to 2")
  else if (length(tau)==2) {
    mf[[1]] <- relogit
    res <- list()
    mf$tau <- min(tau)
    res$lower.estimate <- eval(as.call(mf))
    mf$tau <- max(tau)
    res$upper.estimate <- eval(as.call(mf))
    res$formula <- formula
    class(res) <- c("Relogit2", "Relogit")
    return(res)
  }
  else {
    mf[[1]] <- glm
    mf$family <- binomial(link="logit")
    y2 <- model.response(model.frame(mf$formula, data))
    if (is.matrix(y2))
      y <- y2[,1]
    else
      y <- y2
    ybar <- mean(y)
    if (weighting) {
      w1 <- tau/ybar
      w0 <- (1-tau)/(1-ybar)
      wi <- w1*y + w0*(1-y)
      mf$weights <- wi
    }
    res <- eval(as.call(mf))
    res$call <- match.call(expand.dots = TRUE)
    res$tau <- tau
    X <- model.matrix(res)
    ## bias correction
    if (bias.correct){
      pihat <- fitted(res)
      if (is.null(tau)) # w_i = 1
        wi <- rep(1, length(y))
      else if (weighting) 
        res$weighting <- TRUE
      else {
        w1 <- tau/ybar
        w0 <- (1-tau)/(1-ybar)
        wi <- w1*y + w0*(1-y)
        res$weighting <- FALSE
      }
      W <- pihat * (1 - pihat) * wi
      ##Qdiag <- diag(X%*%solve(t(X)%*%diag(W)%*%X)%*%t(X))
      Qdiag <- lm.influence(lm(y ~ X-1, weights=W))$hat/W
      if (is.null(tau)) # w_1=1 since tau=ybar
        xi <- 0.5 * Qdiag * (2*pihat - 1)
      else
        xi <- 0.5 * Qdiag * ((1+w0)*pihat-w0)
      res$coefficients <- res$coefficients -
        lm(xi ~ X - 1, weights=W)$coefficients
      res$bias.correct <- TRUE
    }
    else
      res$bias.correct <- FALSE
    ## prior correction 
    if (!is.null(tau) & !weighting){      
      if (tau <= 0 || tau >= 1) 
        stop("\ntau needs to be between 0 and 1.\n") 
      res$coefficients["(Intercept)"] <- res$coefficients["(Intercept)"] - 
        log(((1-tau)/tau) * (ybar/(1-ybar)))
      res$prior.correct <- TRUE
      res$weighting <- FALSE
    }
    else
      res$prior.correct <- FALSE
    if (is.null(res$weighting))
      res$weighting <- FALSE

    res$linear.predictors <- t(res$coefficients) %*% t(X) 
    res$fitted.values <- 1/(1+exp(-res$linear.predictors))
    res$zelig <- "Relogit"
    class(res) <- c("Relogit", "glm")
    return(res)
  }
}

#' Zelig2 bridge function
#'
#' ...
#' @note  T
#' @param formula a formula object
#' @param ... ignored parameters
#' @param tau ...
#' @param bias.correct ...
#' @param case.control ...
  #' @param data a data.frame that will be used to fit the model
#' @return a list used internally by zelig
#' @export
zelig2relogit <- function(
                          formula,
                          ...,
                          tau = NULL,
                          bias.correct = NULL,
                          case.control = NULL,
                          data
                          ) {

  # Catch NULL case.control
  if (is.null(case.control))
    case.control <- "prior"

  # Catch NULL bias.correct
  if (is.null(bias.correct))
    bias.correct = TRUE

  # Construct formula. Relogit models have the structure:
  #   cbind(y, 1-y) ~ x1 + x2 + x3 + ... + xN
  # Where y is the response.
  form <- update(formula, cbind(., 1 - .) ~ .)

  # Set the environment to be this function's
  environment(form) <- environment()

  # Return the obvious answer
  z(
    .function = relogit,
    formula = form,
    bias.correct = bias.correct,
    case.control = case.control,
    tau = tau,
    data = data
    )
}
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
param.relogit <- param.logit


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
  object <- obj
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
#' simulate quantities of interest for the zelig ``relogit'' model
#'
#' ...
#' @usage
#' \method{qi}{relogit}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi relogit
#' @param obj a zelig object, containing the fitted ``relogit'' model
#' @param x a ``setx'' object
#' @param x1 a ``setx'' object
#' @param y this parameter is reserved for simulating average treatment effects,
#' though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a ``parameter'' obejct containing information about the link,
#' inverse-link, and simulated parameters
#' @return a param
qi.relogit <- qi.logit


#' simulate quantities of interest for the zelig ``relogit'' model
#'
#' ...
#' @usage
#' \method{qi}{relogit2}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi relogit2
#' @param obj a zelig object, containing the fitted ``relogit'' model
#' @param x a ``setx'' object
#' @param x1 a ``setx'' object
#' @param y this parameter is reserved for simulating average treatment effects,
#' though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a ``parameter'' obejct containing information about the link,
#' inverse-link, and simulated parameters
#' @return a param
qi.relogit2 <- function (obj, x = NULL, x1 = NULL, y = NULL, num=1000, param = NULL) {
  simpar <- param
  # Aliased, because
  object <- obj

  # This model needs work, so it will be discontinued for now
  stop("Relogit 2 is not currently supported")

  num <- nrow(simpar$par0)
  tmp0 <- object$result$lower.estimate
  tmp1 <- object$result$upper.estimate
  
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

    sim01 <- qi.relogit(tmp0, simpar$par0, x = x1, x1 = NULL)
    sim11 <- qi.relogit(tmp1, simpar$par1, x = x1, x1 = NULL)
    tau0 <- object$result$lower.estimate$tau
    tau1 <- object$result$upper.estimate$tau
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

#' Describe a `logit' model to Zelig
#' @usage \method{describe}{relogit}(...)
#' @S3method describe relogit
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.relogit <- function(...) {
  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "dichotomous",
       text = "Rare Events Logistic Regression for Dichotomous Dependent Variables"
       )
}

# Return Names of Relogit Model
#
names.Relogit <- function(x){
  res <- list(default=names(unclass(x)),
            estimate = names(x$lower.estimate), tau = x$tau)
  class(res) <- "names.relogit"
  res
}
