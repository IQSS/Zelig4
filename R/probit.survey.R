#' @export
zelig2probit.survey <- function(
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
    ids <- ~ 1

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
    .survey.prob.weights <- weights
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

  
  z(.function = svyglm,
    formula = formula,
    design  = design,
    family  = quasibinomial(link="probit")
    )
}

#' @S3method param probit.survey
param.probit.survey <- function(obj, num=1000, ...) {
  list(
       simulations = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = NULL,

       # note: assignment of link and link-inverse are
       #       implicit when the family is assigned
       fam   = binomial(link="probit")
       )
}

#' @S3method qi probit.survey
qi.probit.survey <- qi.logit.survey

#' @S3method describe probit.survey
describe.probit.survey <- function(...) {
  list(
       authors = "Nicholas Carnes",
       year = 2008,
       description = "Survey-Weighted Probit Regression for Continuous, Positive Dependent Variables"
       )
}
