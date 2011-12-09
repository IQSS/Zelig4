#' Create Design Matrix of a \code{multiple} Object
#'
#' This method is used to generate a \code{model.matrix} adhering to the
#' specifications in the help document "model.matrix".
#' @usage
#' \method{model.matrix}{multiple}(object,data,shape="compact",eqn=NULL,...)
#' @note This method is scheduled to be deprecated.
#' @param object an object of type \code{multiple}. This represents a Zelig 3.5
#' formula
#' @param data a \code{data.frame}
#' @param shape a character-string specifying the shape of the matrix
#' @param eqn an integer specifying the number of equations
#' @param ... ignored parameters
#' @S3method model.matrix multiple
model.matrix.multiple <- function (object,data,shape="compact",eqn=NULL,...){
  
  intersect <- function(x, y) y[match(x, y, nomatch = 0)]

  #olny for multilevel
  if(class(formula)[[1]]=="terms"){
    terms <-object
  }else{
    terms<-terms(object)
  }
  if(!(is.logical(attr(terms,"subs"))))
    return(multilevel(tt=terms,data=data,eqn=eqn,mode=1))
      
  ##

  if((shape != "compact") && (shape != "array") && (shape !="stacked"))
    stop("wrong shape argument! Choose from \"compact\", \"array\" or \"stacked\" \n")
  
  if(!(any(class(object)=="multiple")))
    stop("Please run first parse.formula() on your formula ...\n")
  
  if(!(any(class(data)=="multiple")))
    data<-model.frame(object,data)
 
 
  terms<-attr(data,"terms")
  
  whiche<-which(eqn %in% names(terms)==FALSE)
  if (length(whiche)!=0)
    stop("Unknown eqn name \"",eqn[whiche],"\"\n")
  
  intercAttr<-attr(terms,"intercept")           
  systEqns<-attr(terms,"systEqns")
  ancilEqns<-attr(terms,"ancilEqns")
  
  if (is.null(eqn))
    eqn=systEqns
  
 # if (!(all(eqn %in% systEqns)))
 #   stop("all eqn names should be from systematic parameters")
  
  termlabels<-attr(terms,"term.labels")[eqn]          
  nrEquations<-length(eqn)
  if (length(eqn)==1)
    shape="compact"
  
  Xnames<-unique(unlist(termlabels))
  
  rhs<-toBuildFormula(Xnames)
  if(!(is.null(rhs)))
    rhs<-as.formula(paste("~",rhs))
  else
    rhs<-as.formula("~1")
  
  rawX<-model.matrix.default(rhs,data=data)
  if (shape=="compact"){
    result<-rawX
    if(all(intercAttr==0)){
      result<-result[,colnames(result)!="(Intercept)"]
    }
    attr(terms,"response")<-0
    attr(result,"terms")<-terms
    return(result)
  }
  
  ronames<-rownames(data)
  ronr<-nrow(data)
  
  parsMat<-make.parameters(terms, shape = "matrix", ancillary = FALSE,eqns=eqn)
  parsVec <- unique(na.omit(c(t(parsMat))))
  
  result<-list()
  result<-array(0,dim=c(ronr,length(parsVec),length(eqn)),dimnames=list(ronames,parsVec,eqn))
  for(i in 1:nrEquations){
    eqni<-eqn[[i]]
    whiche<-which(is.na(parsMat[,eqni])==FALSE)
    result[,,eqni][,parsMat[names(whiche),eqni]]<-rawX[,names(whiche)]
  }
  
  if(shape=="array"){
    res<-result
  }
  if(shape=="stacked"){
    res<-result[,,eqn[[1]]]
    if(length(eqn)>1)
      for(i in 2:length(eqn))
        res<-rbind(res,result[,,eqn[[i]]])
    rownames(res)<-c(1:nrow(res))
  }
  attr(terms,"response")<-0
  attr(res,"terms")<-terms
  return(res)
}

 
