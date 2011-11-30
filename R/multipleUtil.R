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

#mode=1 model.matrix
#mode=2 model.frame
multilevel<-function(tt,data,mode,eqn,...){
  if(!(mode %in% c(1,2)))
    stop("Wrong mode argument")
  if(is.null(eqn))
    stop("Please provide an equations")
res<-list()
  eqns<-attr(tt,"systEqns")
  subs<-attr(tt,"subs")
depVars<-attr(tt,"depVars")

  nrEquations<-length(eqns)
  termlabels<-attr(tt,"term.labels")
#for(i in 1:nrEquations){
  rhs<-toBuildFormula(termlabels[[eqn]],"+")
  if(!is.null(rhs))
    rhs<-paste("~",rhs)
  else
    rhs<-"~1"
  Ynamei<-depVars[[eqn]]
  if(!(Ynamei %in% colnames(subs)))
    lhs<-Ynamei
  else
    lhs<-NULL
  f<-as.formula(paste(lhs,rhs))
  if(mode==1)
    res<-model.matrix.default(f,data)
  #    res[[eqns[[i]]]]<-f
  else
    res<-model.frame.default(f,data)
 # res[[eqns[[i]]]]<-f
#}
attr(res,"terms")<-tt
return(res)
}
