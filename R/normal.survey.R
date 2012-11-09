#' @export
zelig2normal.survey <- function(
                               formula,
                               weights=NULL, 
                               ids=NULL,
                               probs=NULL,
                               strata = NULL,  
                               fpc=NULL,
                               nest = FALSE,
                               check.strata = !nest,
                               repweights = NULL,
                               type,
                               combined.weights=FALSE,
                               rho = NULL,
                               bootstrap.average=NULL, 
                               scale=NULL,
                               rscales=NULL,
                               fpctype="fraction",
                               return.replicates=FALSE,
                               na.action="na.omit",
                               start=NULL,
                               etastart=NULL, 
                               mustart=NULL,
                               offset=NULL, 	      		
                               model1=TRUE,
                               method="glm.fit",
                               x=FALSE,
                               y=TRUE,
                               contrasts=NULL,
                               design=NULL,
                               data
                               ) {

  loadDependencies("survey")

  if (is.null(ids))
    ids <- ~1

  # the following lines designate the design
  # NOTE: nothing truly special goes on here;
  #       the below just makes sure the design is created correctly
  #       for whether or not the replication weights are set
  design <- if (is.null(repweights))
    svydesign(
              data=data,
              ids=ids,
              probs=probs,
              strata=strata,
              fpc=fpc,
              nest=nest,
              check.strata=check.strata,
              weights=weights
              )

  else {
    assign(".survey.prob.weights", weights, envir=.GlobalEnv)
    
    svrepdesign(
                data=data,
                repweights=repweights, 	
                type=type,
                weights=weights,
                combined.weights=combined.weights, 
                rho=rho,
                bootstrap.average=bootstrap.average,
                scale=scale,
                rscales=rscales,
                fpctype=fpctype,
                fpc=fpc
                )
  }

  
  list(
       .function = "svyglm",
       formula = formula,
       design  = design
       )
}

  
#' Param Method for the \code{normal.survey} Zelig Model
#' @note This method is used internally by the \code{survey.zelig} package
#' @S3method param normal.survey
#' @usage \method{param}{normal.survey}(obj, num=1000, ...)
#' @param obj a \code{zelig} object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a \code{parameters} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.normal.survey <- function(obj, num=1000, ...) {
  df <- obj$result$df.residual
  sig2 <- summary(obj)$dispersion
  
  list(
       simulations = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = sqrt(df*sig2/rchisq(num, df=df)),

       # note: assignment of link and link-inverse are
       #       implicit when the family is assigned
       fam   = gaussian()
       )
}
#' Simulate Quantities of Interest for \code{normal.survey} Model
#' @S3method qi normal.survey
#' @usage \method{qi}{normal.survey}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @note This function is paraphrased from Zelig v3.4.0-1
#' @param obj zelig object
#' @param x setx object
#' @param x1 setx object
#' @param y ATT variable
#' @param num implicitly called by sim - number of simulations to run
#' @param param param object contains: link, link-inverse, simulations, ancillary parameters
#' @return a list containing simulated quantities of interest
qi.normal.survey <- function(obj, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  model <- GetObject(obj)

  coef <- coef(param)
  alpha <- alpha(param)

  eta <- coef %*% t(x)

  link.inverse <- linkinv(param)

  theta <- matrix(link.inverse(eta), nrow=nrow(coef))

  pr <- ev <- matrix(NA, nrow=nrow(theta), ncol(theta))

  dimnames(pr) <- dimnames(ev) <- dimnames(theta)


  ev <- theta


  for (k in 1:nrow(ev))
    pr[k, ] <- rnorm(length(ev[k, ]), ev[k,], alpha[k])



  ev1 <- pr1 <- fd <- NA

  if (!is.null(x1)) {
    ev1 <- theta1 <- matrix(link.inverse(coef %*% t(x1)),
                            nrow = nrow(coef)
                            )

    fd <- ev1-ev
  }

  att.ev <- att.pr <- NA

  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(coef)), nrow=nrow(coef), byrow=TRUE)

    tmp.ev <- yvar - ev
    tmp.pr <- yvar - pr

    att.ev <- matrix(apply(tmp.ev, 1, mean), nrow=nrow(coef))
    att.pr <- matrix(apply(tmp.pr, 1, mean), nrow=nrow(coef))
  }


  list(
       "Expected Values: E(Y|X)" = ev,
       "Expected Values for (X1): E(Y|X1)" = ev1,
       "Predicted Values: Y|X" = pr,
       "Predicted Values (for X1): Y|X1" = pr1,
       "First Differences E(Y|X1)-E(Y|X)" = fd,
       "Average Treatment Effect: Y-EV" = att.ev,
       "Average Treatment Effect: Y-PR" = att.pr
       )
}
#' @S3method describe normal.survey
describe.normal.survey <- function(...) {
  list(
       authors = "Nicholas Carnes",
       year = 2008,
       description = "Survey-Weighted Normal Regression for Continuous, Positive Dependent Variables"
       )
}
