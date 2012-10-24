#' Interface between \code{zelig} and \code{svyglm} for the \code{logit.survey}
#' @note This manual file is largely incomplete, and needs a significant amount
#'   of filling out. This, in itself, might be motivation to divide this
#'   package into more models with more specific function.
#' @param formula a \code{formula}
#' @param weights ...
#' @param ids ...
#' @param probs ...
#' @param strata ...
#' @param fpc ...
#' @param nest ...
#' @param check.strata ...
#' @param repweights ...
#' @param type ...
#' @param combined.weights ...
#' @param rho ...
#' @param bootstrap.average ...
#' @param scale ...
#' @param rscales ...
#' @param fpctype ...
#' @param return.replicates ...
#' @param na.action ...
#' @param start ...
#' @param etastart ...
#' @param mustart ...
#' @param offset ...
#' @param model1 ...
#' @param method ...
#' @param x ...
#' @param y ...
#' @param contrasts ...
#' @param design ...
#' @param data a \code{data.frame}
#' @return a \code{list} used to construct parameters for the \code{svyglm}
#' function
#' @export
zelig2logit.survey <- function(
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

  Zelig:::loadDependencies(survey)

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

  # formals(glm)$family <- "SPAGHETTI!!"

  # we cannot plug in family=Gamma yet because of weird issues
  # with glm. Uncomment the below lines for an explanation:

  ## fails:
  # test <- Gauss
  # svyglm(formula=formula, design=design, family=test)

  ## works:
  # svyglm(formula=formula, design=design, family=Gauss)

  # this is because of how glm is written (it evaluates the
  # family variable as a function in the parent.frame)

  list(
      .function = "svyglm",
       formula = formula,
       design  = design,
       family  = quasibinomial(link="logit")
       )
}

#' @S3method param logit.survey
param.logit.survey <- function(obj, num=1000, ...) {
  list(
       simulations = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = NULL,
       fam   = binomial(link="logit")
       )
}

#' @S3method qi logit.survey
qi.logit.survey <- function(obj, x, x1=NULL, y=NULL, num=1000, param=NULL) {

  model <- GetObject(obj)

  coef <- coef(param)
  alpha <- alpha(param)

  eta <- coef %*% t(x)

  link.inverse <- linkinv(param)

  theta <- matrix(link.inverse(eta), nrow=nrow(coef))

  pr <- ev <- matrix(NA, nrow=nrow(theta), ncol(theta))

  dimnames(pr) <- dimnames(ev) <- dimnames(theta)

  ev <- theta

  for (k in 1:ncol(theta)) {
    pr[,k] <- rbinom(length(ev[,k]), 1, ev[,k])
    pr[,k] <- as.character(pr[,k])
  }

  levels(pr) <- c("0", "1")
  
  if (!is.null(y) && NCOL(y))
    y <- y[,1]


  # invisiblify 
  pr1 <- ev1 <- fd <- rr <- NA

  
  if (!is.null(x1)) {
    ev1 <- theta1 <- matrix(link.inverse(coef %*% t(x1)),
                            nrow = nrow(coef)
                            )


    pr1 <- matrix(NA, nrow=nrow(theta), ncol(theta))

    for (k in 1:ncol(theta)) {
      pr1[,k] <- rbinom(length(ev1[,k]), 1, ev1[,k])
      pr1[,k] <- as.character(pr1[,k])
    }

    levels(pr1) <- c("0", "1")
    
    fd <- ev1-ev
    rr <- ev1/ev
  }


  att.ev <- att.pr <- NA

  if (!is.null(y)) {

    yvar <- matrix(rep(y, nrow(coef)),
                   nrow = nrow(coef)
                   )

    tmp.ev <- yvar - ev
    tmp.pr <- yvar - as.integer(pr)

    att.ev <- matrix(apply(tmp.ev, 1, mean), nrow=nrow(coef))
    att.pr <- matrix(apply(tmp.pr, 1, mean), nrow=nrow(coef))
  }

  list(
       "Expected Values: E(Y|X)" = ev,
       "Expected Values (for x1): E(Y|X1)" = ev1,
       "Predicted Values: Y|X" = pr,
       "Predicted Values (for x1): Y|X1" = pr1,
       "First Differences: E(Y|X1)-E(Y|X)" = fd,
       "Risk Ratios: P(Y=1|X1)/P(Y=0|X)" = rr,
       "Average Treatment Effect: Y - EV" = att.ev,
       "Average Treatment Effect: Y - PR" = att.pr
       )
}

#' @S3method describe logit.survey
describe.logit.survey <- function(...) {
  list(
       authors = "Nicholas Carnes",
       year = 2008,
       description = "Survey-Weighted Logitistic Regression for Continuous, Positive Dependent Variables"
       )
}
