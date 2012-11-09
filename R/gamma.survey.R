#' Interface between \code{zelig} and \code{svyglm} for the \code{gamma.survey}
#' @note This manual file is largely incomplete, and needs a significant amount
#'   of filling out. This, in itself, might be motivation to divide this
#'   package into more models with more specific function.
#' @param formula a \code{formula}
#' @param weights NULL
#' @param ids NULL
#' @param probs NULL
#' @param strata NULL  
#' @param fpc NULL
#' @param nest FALSE
#' @param check.strata !nest
#' @param repweights NULL
#' @param type ... ...
#' @param combined.weights FALSE
#' @param rho   NULL
#' @param bootstrap.average NULL
#' @param scale NULL
#' @param rscales NULL
#' @param fpctype "fraction"
#' @param return.replicates FALSE
#' @param na.action "na.omit"
#' @param start NULL
#' @param etastart NULL
#' @param mustart NULL
#' @param offset NULL	      		
#' @param model1 TRUE
#' @param method "glm.fit"
#' @param x FALSE
#' @param y TRUE
#' @param contrasts NULL
#' @param design NULL
#' @param link "inverse"
#' @param data a \code{data.frame}
#' @param ... ignored parameters
#' @return a \code{list} used to construct parameters for the \code{svyglm}
#'   function
#' @export
zelig2gamma.survey <- function(
                               formula,
                               weights=NULL, 
                               ids=NULL,
                               probs=NULL,
                               strata = NULL,  
                               fpc = NULL,
                               nest = FALSE,
                               check.strata = !nest,
                               repweights = NULL,
                               type,
                               combined.weights = FALSE,
                               rho = NULL,
                               bootstrap.average = NULL, 
                               scale = NULL,
                               rscales = NULL,
                               fpctype = "fraction",
                               return.replicates=FALSE,
                               na.action = "na.omit",
                               start = NULL,
                               etastart = NULL, 
                               mustart = NULL,
                               offset = NULL, 	      		
                               model1 = TRUE,
                               method = "glm.fit",
                               x = FALSE,
                               y = TRUE,
                               contrasts = NULL,
                               design = NULL,
                               link = "inverse",
                               data,
                               ...
                               ) {

  loadDependencies("survey")

  if (is.null(ids))
    ids <- ~1

  # the following lines designate the design
  # NOTE: nothing truly special goes on here;
  #       the below just makes sure the design is created correctly
  #       for whether or not the replication weights are set
  design <- if (is.null(repweights)) {
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
  }

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

  list(.function = "svyglm",
       
       formula = formula,
       design  = design,
       family  = Gamma()
       )
}

#' @S3method param gamma.survey
param.gamma.survey <- function(obj, num=1000, ...) {
  shape <- gamma.shape(.fitted)

  list(
       # .fitted is the fitted model object
       simulations = mvrnorm(num, coef(.fitted), vcov(.fitted)),
       alpha = rnorm(num, shape$alpha, shape$SE),
       fam   = Gamma()
       )
}

#' @S3method qi gamma.survey
qi.gamma.survey <- function(obj, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  model <- GetObject(obj)

  coef <- coef(param)
  alpha <- alpha(param)

  eta <- coef %*% t(x)

  link.inverse <- linkinv(param)

  theta <- matrix(link.inverse(eta), nrow=nrow(coef))

  pr <- ev <- matrix(NA, nrow=nrow(theta), ncol(theta))

  dimnames(pr) <- dimnames(ev) <- dimnames(theta)


  ev <- theta

  for (i in 1:nrow(ev)) {
    pr[i,] <- rgamma(
                     n     = length(ev[i,]),
                     shape = alpha[i],
                     scale = theta[i,]/alpha[i]
                     )
  }


  # ensure these are no-show
  pr1 <- ev1 <- fd <- NA

  
  # if x1 is available
  if (!is.null(x1)) {
    ev1 <- theta1 <- matrix(link.inverse(coef %*% t(x1)), nrow(coef))
    fd <- ev1-ev
  }


  # ensure these are no-show
  att.pr <- att.ev <- NA


  # I have no clue if this even works
  if (!is.null(y)) {

    yvar <- matrix(
                   rep(y, nrow(param)),
                   nrow = nrow(param),
                   byrow = TRUE
                   )
    
    tmp.ev <- yvar - ev
    tmp.pr <- yvar - pr

    att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(param))
    att.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(param))
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

#' @S3method describe gamma.survey
describe.gamma.survey <- function(...) {
  list(
       authors = "Nicholas Carnes",
       year = 2008,
       description = "Survey-Weighted Gamma Regression for Continuous, Positive Dependent Variables"
       )
}
