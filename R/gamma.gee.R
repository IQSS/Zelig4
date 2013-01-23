#' Interface between the Zelig Model gamma.gee and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param id a character-string specifying the column of the data-set to use
#'   for clustering
#' @param robust a logical specifying whether to robustly or naively compute
#'   the covariance matrix. This parameter is ignore in the \code{zelig2}
#'   method, and instead used in the \code{robust.hook} function, which
#'   executes after the call to the \code{gee} function
#' @param ... ignored parameters
#' @param R a square-matrix specifying the correlation
#' @param corstr a character-string specifying the correlation structure
#' @param data a data.frame 
#' @return a list specifying the call to the external model
#' @export
zelig2gamma.gee <- function (formula, id, robust = FALSE, ..., R = NULL, corstr = "independence", data) {

  loadDependencies("gee")

  if (corstr == "fixed" && is.null(R))
    stop("R must be defined")

  # if id is a valid column-name in data, then we just need to extract the
  # column and re-order the data.frame and cluster information
  if (is.character(id) && length(id) == 1 && id %in% colnames(data)) {
    id <- data[, id]
    data <- data[order(id), ]
    id <- sort(id)
  }

  z(
    .function = gee,
    .hook = robust.gee.hook,

    formula = formula,
    id = id,
    corstr = corstr,
    family  = Gamma,
    data = data,
    ...
    )
}

#' @S3method param gamma.gee
param.gamma.gee <- function(obj, num=1000, ...) {

  # Extract means to compute maximum likelihood
  mu <- coef(.fitted)

  # Extract covariance matrix to compute maximum likelihood
  Sigma <- .fitted$naive.variance


  #
  list(
       coef = mvrnorm(num, mu, Sigma),
       fam = Gamma()
       )
}

#' @S3method qi gamma.gee
qi.gamma.gee <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  coef <- coef(param)
  inverse <- linkinv(param)

  eta1 <- coef %*% t(x)
  ev1 <- theta1 <- matrix(inverse(eta1), nrow=num)

  # default to NA
  ev2 <- fd <- NA

  if (!is.null(x1)) {
    eta2 <- coef %*% t(x1)
    ev2 <- theta1 <- matrix(inverse(eta2), nrow=num)

    fd <- ev2 - ev1
  }

  list(
       "Expected Values (for x): E(Y|X)"   = ev1,
       "Expected Values (for x1): E(Y|X1)" = ev2,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}

#' @S3method describe gamma.gee
describe.gamma.gee <- function(...) {
  list(
       authors = "Patrick Lam",
       text = "General Estimating Equation for Gamma Regression",
       year = 2011
       )
}

# Remove Negative Simulations from Gamma GEE Parameter Simulations
# @param object a \code{zelig} object
# @param x a \code{setx} object
# @param x1 a \code{setx} object
# @param bootstrap a logical specifying whether the model is using a boot function
# @param bootfn the boot function
# @param data a data.frame used to simulated parameters
# @param param the original \code{param} object
# @param num an integer specifying the number of simulations to produce
clean.up.gamma.gee <- function(object, x, x1=NULL,
                            bootstrap = FALSE, bootfn = NULL,
                            data = NULL,
                            param, num = 1000) {
  coef <- coef(param)
  eta <- coef %*% t(x)

  if(!is.null(x1))
    eta1 <- coef %*% t(x1)
  else
    eta1 <- NULL

  # define good.parameters (???)
  good.params <- function(par, x, x1=NULL) {
    eta <- par %*% t(x)
    if(!is.null(x1)) {
      eta1 <- par %*% t(x1)
      pos <- which(eta>0 & eta1>0)
    }
    else {
      pos <- which(apply(eta > 0,1,all))
    }

    matrix(par[pos,], nrow=length(pos), ncol=ncol(par))
  }



      if(length(which(apply(eta<=0,1,any)))>0 | (!is.null(eta1) & any(eta1<=0))){
              warning(paste("Negative expected values in simulations.  Rejection sampling method used."))
              sum.neg <- length(which(apply(eta<=0,1,any)))
              coef <- good.params(par=coef, x=x, x1=x1)
              counter <- 1
              while(sum.neg > 0){
                      if(!bootstrap)
                              new.coef <- matrix(mvrnorm(sum.neg, mu = coef(object), Sigma = vcov(object)), nrow=sum.neg)
			#else
			#	new.coef <- matrix(boot(data, bootfn, R = sum.neg, object = object)$t, nrow=sum.neg)
				
			new.coef <- good.params(par=new.coef, x=x, x1=x1)
			coef <- rbind(coef, new.coef)	
			sum.neg <- num - nrow(coef)
			counter <- counter + 1
			if(counter==200)
				warning(paste("Suitable parameters not found after 200 iterations of rejection sampling.  Iterations will continue, but choosing another x is suggested for non-conditional prediction models."))
			if(counter==2000)
				stop("Rejection sampling stopped after 2000 iterations.  Please choose another x value.")
		}
	}

  #
  list(
       coefficients=coef,
       fam=Gamma(),
       linkinv = Gamma()$linkinv
       )
}
