cmvglm <- function(formula, model, ndim,data=NULL, fact=NULL){

  toBuildFormula<-function(Xnames,sepp="+"){
    lng<-length(Xnames)
    rhs<-NULL
    if (lng!=0){
      if(lng==1){
        rhs=Xnames
      }else{
        for (j in 1:(lng-1)){
          rhs<-paste(rhs,as.name(Xnames[[j]]))
          rhs<-paste(rhs,sepp)
        }
        rhs<-paste(rhs,Xnames[[lng]])
      }
    }
    return (rhs)
  }
  tt<-terms(formula)
  attr(tt,"systEqns")<-names(formula)
  p<-make.parameters(tt,shape="matrix")
  vars<-rownames(p)
  cm<-vector("list", length(vars))
  names(cm)<-vars
  
    for(i in 1:length(cm))
      cm[[i]]<-diag(1, ndim)

  constrain<-attr(tt,"constraints")
  if(!is.logical(constrain)){
    tmp <- sort(colnames(constrain))
    for (i in 1:length(tmp)) {
      ci<-constrain[,i]
      if (is.null(na.omit(ci)) || length(unique(na.omit(ci)))!=1)
        stop("invalid input for constrain")
      minj <- match(FALSE, is.na(ci))
      whatvar <- pmatch(unique(na.omit(ci)), names(cm))
      for (j in 1:3)
        if (!is.na(ci[j])) {
          cm[[whatvar]][j,j]<-0
          cm[[whatvar]][j,minj]<-1
        }
    }
  }
  for(i in rownames(p)){
    for(j in 1:ncol(p)){
      if(is.na(p[i,j]))
        cm[[i]][j,j]<-0
    }
  }
    
 # if(!is.null(constant))
 #   for(i in 1:length(constant))
 #     for(j in 1:length(cm))
 #       if(names(cm)[j]!="(Intercept)")
 #         cm[[j]][constant[i],]<-matrix(0, ncol=ncol(cm[[j]]))

  for(i in 1:length(cm))
    cm[[i]]<-as.matrix(cm[[i]][,apply(cm[[i]], 2, sum)!=0])
  rhs<-toBuildFormula(attr(tt,"indVars"))
  if(!(is.null(rhs)))
    rhs<-(paste("~",rhs))
  else
    rhs<-"~1"
  Ynames<-unlist(attr(tt,"depVars"))
  if(!is.null(fact))
    lhs<-fact
  else{
    if(length(Ynames)>1){
      lhs<-toBuildFormula(Ynames,",")
      if (!(is.null(lhs))){
        lhs<-paste("cbind(",lhs)
        lhs<-paste(lhs,")")
      }
    }else{
      lhs=Ynames
    }
  }
  formula<-as.formula(paste(lhs,rhs))
  list("formula"=formula, "constraints"=cm)
}
