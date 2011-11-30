#' @S3method model.frame multiple
model.frame.multiple <- function (formula,data,eqn=NULL,...){
  if(class(formula)[[1]]=="terms"){
    terms <-formula
  }else{
    terms<-terms(formula)
  }

  #is multilevel?
  if(!(is.logical(attr(terms,"subs"))))
    return(multilevel(tt=terms,data=data,eqn=eqn,mode=2))


  "%w/o%" <- function(x,y) x[!x %in% y]


  eqns<-names(formula)
  eqns<-attr(terms,"systEqns")
  nrEquations<-length(eqns)
  termlabels<-attr(terms,"term.labels")
  depVars<-attr(terms,"depVars")
  Xs<-Ys<-tlNew<-dvNew<-list()
  for (i in 1:nrEquations){
    rhs<-toBuildFormula(termlabels[[eqns[[i]]]])
    if(!(is.null(rhs))){
      rhs<-paste(rhs,"-1")
      rhs<-as.formula(paste("~",rhs))
     Xs[[eqns[[i]]]]<-model.matrix.default(rhs,data=data)
      tlNew[[eqns[[i]]]]<-colnames(Xs[[eqns[[i]]]])
      tlNew[[eqns[[i]]]]<-gsub("as.factor\\(.*\\)","",tlNew[[eqns[[i]]]])
      colnames(Xs[[eqns[[i]]]])<-tlNew[[eqns[[i]]]]
    }
  }
  depFactors<-attr(terms,"depFactors")
 
  if(!(is.logical(depFactors)))
    depVars<- paste("as.factor(",depFactors[[1]],")",sep="")
  #print(depVars)
  lhs<-toBuildFormula(unique(unlist(depVars)))
  if(!(is.null(lhs))){
    lhs<-paste(lhs,"-1")
    lhs<-as.formula(paste("~",lhs))
    Ys<-model.matrix.default(lhs,data=data)
    dvNew<-colnames(Ys)
    dvNew<-gsub("as.factor\\(.*\\)","",dvNew)
    colnames(Ys)<-dvNew
  }
  attr(terms,"term.labels")[names(tlNew)]<-tlNew
  attr(terms,"depVars")[names(dvNew)]<-dvNew

  ronames<-rownames(data)
  ronr<-nrow(data)
  Xnames<-unique(unlist(tlNew))
  Ynames<-unique(unlist(dvNew))
  if(!(is.logical(depFactors)))
    Ynames<-c(depFactors[[2]],Ynames %w/o% depFactors[[2]])
  X<-matrix(0,nrow=ronr,ncol=length(Xnames),dimnames=list(ronames,Xnames))
  Y<-matrix(0,nrow=ronr,ncol=length(Ynames),dimnames=list(ronames,Ynames))
  if(length(tlNew)>0)
  for(i in 1:length(tlNew)){
    xtmp<-intersect(tlNew[[i]],Xnames)
    X[,xtmp]<-Xs[[i]][,xtmp]
  }
  Y<-Ys
  my.data.frame<-as.data.frame(cbind(Y,X))
  rhs<-toBuildFormula(Xnames)
  if(!(is.null(rhs)))
    rhs<-(paste("~",rhs))
  else
    rhs<-"~1"
  cb<-FALSE
  if(length(Ynames)>1){
    lhs<-toBuildFormula(Ynames,",")
    if (!(is.null(lhs))){
      lhs<-paste("cbind(",lhs)
      lhs<-paste(lhs,")")
      cb<-TRUE
    }
  }else{
    lhs=Ynames
  }
  lhs<-as.formula(paste(lhs,rhs))
  Y<-model.frame.default(lhs,data=my.data.frame)
  result=Y
  if(cb)
    names(result)[[1]]<-"response"
  new.response<-attr(attr(result,"terms"),"response")
  attr(terms,"response")<-new.response
  attr(result,"terms")<-terms
  class(result)<-c(class(result),"multiple")
  return(result)

}



