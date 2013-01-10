#' @export
zelig2mlogit.bayes <- function (
                               formula, 
                               burnin = 1000, mcmc = 10000, 
                               verbose=0, 
                               ..., 
                               data
                               ) {

  loadDependencies("MCMCpack", "coda")

  list(
       .function = "MCMCmnl",
       .hook = "MCMChook",

       formula = formula,
       data   = data,
       burnin = burnin,
       mcmc   = mcmc,
       verbose= verbose,

       # Most parameters can be simply passed forward
       ...
       )
}

#' @S3method param mlogit.bayes
param.mlogit.bayes <- function(obj, num=1000, ...) {
  list(
       coef = coef(obj),
       linkinv = NULL
       )
}

#' @S3method qi mlogit.bayes
qi.mlogit.bayes <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  res1 <- compute.mlogit.bayes(.fitted, x, y, num, param)
  res2 <- compute.mlogit.bayes(.fitted, x1, y, num, param)

  list(
       "Expected Value: E(Y|X)" = res1$ev,
       "Predicted Value: Y|X"   = res1$pv,
       "Expected Value (for X1): E(Y|X1)" = res2$ev,
       "Predicted Value (for X1): Y|X1"   = res2$pv,
       "First Differences"   = res2$ev - res1$ev
       )
}

compute.mlogit.bayes <- function (obj, x, y, num, param) {
  # If either of the parameters are invalid,
  # Then return NA for both qi's
  if (is.null(x) || is.na(x) || is.null(param))
    return(list(ev=NA, pv=NA))

  # 
  resp <- model.response(model.frame(obj))

  level <- length(table(resp))
  p <- dim(model.matrix(eval(obj),data=obj$data))[2]
  coef <- coef(obj)
  eta <- array(NA, c(nrow(coef),level, nrow(x$matrix)))



  eta[, 1, ] <- matrix(0, nrow(coef), nrow(x$matrix))

  for (j in 2:level) {
    ind <- (1:p)*(level-1)-(level-j)
    eta[,j,]<- coef[,ind]%*%t(x)
  }

  eta<-exp(eta)
  ev <- array(NA, c(nrow(coef), level, nrow(x$matrix)))
  pr <- matrix(NA, nrow(coef), nrow(x$matrix))
  colnames(ev) <- rep(NA, level)

  for (k in 1:nrow(x$matrix)) {
    for (j in 1:level)
      ev[,j,k] <- eta[,j,k]/rowSums(eta[,,k])
  }

  for (j in 1:level) {
    colnames(ev)[j] <- paste("P(Y=", j, ")", sep="")
  }

  for (k in 1:nrow(x$matrix)) {             
    probs <- as.matrix(ev[,,k])
    temp <- apply(probs, 1, FUN=rmultinom, n=1, size=1)
    temp <- as.matrix(t(temp)%*%(1:nrow(temp)))
    pr <- apply(temp,2,as.character)
  }
  list(ev = ev, pv = pr)
}

#' @S3method describe mlogit.bayes
describe.mlogit.bayes <- function(...) {
  list(
       authors = c("Ben Goodrich", "Ying Lu"),
       text = "Bayesian Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values",
       year = 2013
       )
}
