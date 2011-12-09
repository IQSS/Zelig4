#' Build Formula ???
#' 
#' This function builds a formula
#' @param Xnames a character-vector
#' @param sepp a seperator (???)
#' @return a character-string
#' @author ???
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


#' Multilevel
#' 
#' This function currently has no documentation, but is essential in Zelig 3.5's
#' implementation of formulae.
#' @param tt a terms object
#' @param data a \code{data.frame}
#' @param mode ???
#' @param eqn an integer specifying the number of equations in a model
#' @param ... ignored parameters
#' @return a list with the "terms" attribute specified
#' @author Kosuke Imai, Olivia Lau, Gary King and Ferdinand Alimadhi
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
