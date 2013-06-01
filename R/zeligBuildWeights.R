#' Utility to build a vector (or sometimes matrix) of weights for analysis model.
#' 
#' This takes standardized Zelig user input about weights, and tailors it
#'   via developer defined settings, to correspond with the format of 
#'   weights acceptable by the model Zelig bridges to.  It also runs a
#'   set of checks to uncover any potential errors in the specified weights.
#' @param weights A set of non-negative value weights.  Overrides repweights 
#'   if defined.
#' @param repweights A set of whole number (non-negative integer) weights.  
#'   useful if weights are just for making copies or deleting certain 
#'   or frequency weights.
#' @param zeros An option on how to deal with zero valued user supplied weights.
#'   Default of "zero" allows zero weights, "epsilon" changes zeroes to 1e-08,
#'   "remove" removes those observations from the dataset.
#' @param rebuild An option to allow specified repweights to reconfigure the 
#'   rows of the dataset to rebuild a corresponding dataset where every row is
#'   of weight 1.  Useful if analysis model does not accept weights.  
#' @param data Dataset, required if weights are defined by variable name, or if
#'   dataset is to be reconfigured (by \code{rebuild} or \code{zeros} options)
#' @param useweights Defines if weights are allowed in model.
#' @param userepweights Defines if repweights are allowed in model.  Overridden if
#'   \code{useweights=TRUE}.
#'   dataset is to be reconfigured (by \code{rebuild} or \code{zeros} options)
#' @param data Dataset, required if weights are defined by variable name, or if
#'   dataset is to be reconfigured (by \code{rebuild} or \code{zeros} options)
#' @return weights A vector of weights of the structure defined by the
#'   developer and required by the analysis model.  Or NULL if certain checks 
#'   are failed.
#' @return data A reconfigured dataset, if modified.
#' @author James Honaker \email{zelig-zee@@iq.harvard.edu}
#' @export

zeligBuildWeights <- function (weights=NULL, repweights=NULL, zeros="zeros", rebuild=FALSE, allowweights=TRUE, allowrepweights=TRUE, data=NULL) {


  ## Developer can turn off certain types of weights
  ## NOTE: Can't currently turn off "repweights", if "weights" allowable.
  if(!allowweights & !allowrepweights & !is.null(weights)){
    warning("You have specified weights, but weighting is not available for this model.  Ignoring weights.  ")
    return(list(weights=NULL, data=data))
  }

  if(!allowweights & !allowrepweights & !is.null(repweights)){
    warning("You have specified repweights, but weighting is not available for this model.  Ignoring weights.  ")
    return(list(weights=NULL, data=data))
  }

  if(!allowweights & !is.null(weights)){
    warning("You have specified weights, but weights are not an option in this model.  Ignoring weights.  repweights may be an available option.")
    weights=NULL
  }


  ## Override repweights with weights when in conflict.
  if(!is.null(weights) & !is.null(repweights)){
    warning("You have specified both weights and repweights.  The repweights will be ignored.")
    repweights<-NULL
  }


  ## Turn weights as variable name into vector, with checking.
  if(is.character(weights)){
    if(is.null(data)){
      warning("ZELIG DEVELOPER WARNING: You have named a weight variable in the dataset, but not supplied dataset to zeligBuildWeights.  Weights will be ignored in your model until amended.")
      return(list(weights=NULL, data=data))
    }else if (!(weights %in% names(data))){
      warning("The variable name supplied for the weights is not present in the dataset. Ignoring weights.")
      return(list(weights=NULL, data=data))
    }else if ( !is.numeric(data[,weights])){
      warning("The variable supplied for the weights is not numeric. Ignoring weights.")
      return(list(weights=NULL, data=data))
    }else{
      weights<-data[,weights]
    }
  }

  ## Turn repweights as variable name into vector, with checking.
  if(is.character(repweights)){
    if(is.null(data)){
      warning("ZELIG DEVELOPER WARNING: You have named a repweight variable in the dataset, but not supplied dataset to zeligBuildWeights.  repweights will be ignored in your model until amended.")
      return(list(weights=NULL, data=data))
    }else if (!(repweights %in% names(data))){
      warning("The variable name supplied for the repweights is not present in the dataset. Ignoring weights.")
      return(list(weights=NULL, data=data))
    }else if ( !is.numeric(data[,repweights])){
      warning("The variable supplied for the repweights is not numeric. Ignoring weights.")
      return(list(weights=NULL, data=data))
    }else{
      repweights<-data[,repweights]
    }
  }

  ## Some checking/transforming on repweights
  if(!is.null(repweights)){
    if(!all(floor(repweights)==repweights)){
      warning("Defined repweights are not integer, so will be rounded.")
      repweights=round(repweights)   # Maybe allow floor/ceiling as other options?
    }
    if(sum(is.na(repweights))>0){  # any(is.na()) sometimes has issues
      warning("Some defined repweights are missing values, so will be treated as zeros")
      flag<-is.na(repweights)
      repweights[flag]<-0;
    }
    if(min(repweights)<0){
      warning("Some defined repweights are negative, so will be treated as zeros")
      flag<-repweights<0
      repweights[flag]<-0;
    }
    if(sum(repweights)==0){
      warning("Defined repweights give no weight to any observation.  Ignoring weights.")
      return(list(weights=NULL, data=data))
    }
  }


  ## Some checking/transforming on weights
  if(!is.null(weights)){
    if(sum(is.na(weights))>0){  # any(is.na()) sometimes has issues
      warning("Some defined weights are missing values, so will be treated as zeros")
      flag<-is.na(weights)
      weights[flag]<-0;
    }
    if(min(weights)<0){
      warning("Some defined weights are negative, so will be treated as zeros")
      flag<-weights<0
      weights[flag]<-0;
    }
    if(sum(weights)==0){
      warning("Defined weights give no weight to any observation.  Ignoring weights.")
      return(list(weights=NULL, data=data))
    }
  }


  ## If repweights not available to function, reconstruct a dataset
  ## NOTE: "rebuild" overrides any setting of "zeros"
  if( is.null(weights) & !is.null(repweights) & rebuild){
    if(is.null(data)){
      warning("ZELIG DEVELOPER WARNING: You have set zeligBuildWeights to rebuild dataset, but not supplied dataset to zeligBuildWeights function.  Weights will be ignored in your model until amended.")
      return(list(weights=NULL, data=data))
    }else{
      ##  Rebuild dataset according to replication weights
      newobs<-rep(1:nrow(data), repweights)  # Index of rows to use
      data<-data[newobs,]                    # Copy relevant rows
    }
    weights<-NULL # Or, could be weights<-rep(1,nrow(data))
  ## when repweights are correct, but need to be transfered to final output
  }else if (is.null(weights) & !is.null(repweights) ){
    weights<-repweights
  }

  ## From this point, only "weights" exists in a meaningful way.

  if(!is.null(weights)){
    ## Implement zeros option.
    if(zeros=="epsilon"){
      flag<-weights==0
      weights[flag]<- .00000001
    }else if (zeros=="remove"){
      flag<-weights==0
      weights<-weights[!flag]
      data<-data[!flag,]
    }
  }



  ## NOTE: Ideally, we could just pass back a vector of indexes to reformat the 
  ##   data, rather than passing back and forwards the data.
  ##   But simpler for developer this way.  

  built <- list(weights=weights, data=data)

  # Return
  return(built)
}


