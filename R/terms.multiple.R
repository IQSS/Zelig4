###
## terms
## -accept single and multiple equations:
## -in case of single equations, the equation is named "mu". is this right?
## -if mu=y~x:z then the attr(tt,"variable") gives list(y,x:z). Should it be list(y,x,z) ??
## -


terms.multiple<-function(x, data=NULL,...){
        object <- x
        termsexist<-attr(object,"terms")
        if(!(is.null(termsexist)))
          return (termsexist)
        
        nreq<-nrConstr<-nrEquationsNew<-0
        constr<-XconstrEqn<-variables<-termlabels<-depVars<-objectNew<-intercAttr<-depFactors<-list()
        depFactorVar<-depLevels<-namesConstr<-c()
        if(!(any(class(object)=="list"))){
                object<-list(object)
                names(object)<-"mu"
        }
        namesOfEquations<- names(object)
        nrEquations <-length(object)
        "%w/o%" <- function(x,y) x[!x %in% y]
        
        for (i in 1:nrEquations){
                TT<-terms.formula(object[[i]], specials=c("id","tag"))               
                attrTTvars<-attr(TT,"variables")
                attrTTlabels<-attr(TT,"term.labels")
                
                eqni<-object[[i]]                    
                namei<-namesOfEquations[[i]]            
                tagattr<-attr(TT,"specials")$tag         
                hastag<-!(is.null(tagattr))
                if (hastag){
                        ## has tag so make a new list of variables and term.labels
                        newVars<-list()           
                        newLabels<-c()
                        indxV<-indxL<-1
                        constrTmp<-c()
                        for(j in 1:length(tagattr)){
                                taglabels<-c()
                                if(length(eqni)==3)
                                  lind<-tagattr[[j]]-1
                                else
                                  lind<-tagattr[[j]]
                                vind<-tagattr[[j]]+1
                                ## add all vars/terms prior to tag into new list of
                                ## newVars and newLabels
                                for(v in indxV:(vind))
                                  newVars<-c(newVars,attrTTvars[[v]])
                                newVars[[length(newVars)]]<-NULL
                                indxV<-vind+1
                                
                                for(l in c(indxL:lind))
                                  newLabels<-c(newLabels,attrTTlabels[[l]])
                                newLabels<-newLabels[-(length(newLabels))]
                                indxL<-lind+1
                                
                                ## deparse and fix the tag
                                tagAsList <-.fixTag(.deparseTag(attrTTvars[[vind]]))
                                for (tindx in 1:length(tagAsList)){
                                        t<-tagAsList[[tindx]]
                                        if(((t$var %in% namesOfEquations)==FALSE) && t$var != "none" && t$var != "1"){
                                                newVars<-c(newVars,parse(text=t$var)[[1]])
                                                newLabels<-c(newLabels,t$var)
                                        }
                                        if(((t$id %in% namesOfEquations)==FALSE) && t$id !="none" && t$id !="1"){
                                                ##print(t$id)
                                                newVars<-c(newVars,parse(text=t$id)[[1]])
                                                newLabels<-c(newLabels,t$id)
                                        }
                                        ## constraints ?
                                        if(t$var !="none" && t$label !="none" && t$id =="none"){
                                                nrConstr<-nrConstr+1
                                                namesConstr<-c(namesConstr,t$label)
                                                constr[[nrConstr]]<-c(i,t$label,t$var)
                                                constrTmp<-c(constrTmp,t$var)   ##???? what is constrTMP?
                                        }
                                }
                        }
                        ## if there is any var/term remaining after tags
                        ## add them to newVars and newLabels
                        if(length(attrTTvars)>vind){
                                for(v in (vind+1):length(attrTTvars))
                                  newVars<-c(newVars,attrTTvars[[v]])
                        }
                        
                        if(length(attrTTlabels)>lind){
                                for(l in (lind+1):length(attrTTlabels))
                                  newLabels<-c(newLabels,attrTTlabels[[l]])
                        }
                        
                        XconstrEqn[[i]]<-constrTmp

                        ## make newVars and newLabels unique
                        newVars<-unique(newVars)  
                        newLabels <- unique(newLabels)
                } else{
                        ## there is no tag => newVars and newLabels remain unchanged
                        newVars<-attrTTvars
                        newLabels<-attrTTlabels
                }
                nrEquationsNew<-nrEquationsNew+1
                objectNew[[namei]]<-eqni
                if (length(eqni)==3){

                        nreq=nreq+1    ## number of required equations
                        lhs<-eqni[[2]]
                        if (length(lhs)>1 && lhs[[1]]=="id"){
                                depVars[[namei]]<-lhs[[3]]
                                depFactorVar<-c(depFactors,deparse(lhs[[2]]))
                                depLevels<-c(depLevels,lhs[[3]])
                        }else
                        depVars[[namei]]<-deparse(eqni[[2]])
                        
                }
                attr(TT,"variables")<-as.call(newVars)
                attr(TT,"term.labels")<-newLabels
                variables[[namei]]<-attr(TT,"variables")
                termlabels[[namei]]<-attr(TT,"term.labels")
                intercAttr[[namei]]<-attr(TT,"intercept")
        }  ## end of for each equation
        
        namesOfEquations<-names(objectNew)
        myattr<-list()
        result<-objectNew
        constraints<-subs<-FALSE

        ## construct constraints
        namesConstr<-unique(namesConstr)
        if(length(constr)>0){
                constraints<-matrix(NA,nrow=nrEquationsNew,ncol=length(namesConstr),dimnames=list(namesOfEquations,namesConstr))
                for(i in 1:length(constr)){
                        constri<-constr[[i]]
                        eqind<-constri[[1]]
                        eq<-namesOfEquations[as.numeric(eqind)]
                        lab<-constri[[2]]
                        constraints[eq,lab]<-constri[[3]]
                }
        }
        
        indVars<-unique(unlist(termlabels))
        if(length(depFactorVar) !=0)
          depFactors<-list("depFactorVar"=unique(unlist(depFactorVar)),"depLevels"=depLevels)
        else
          depFactors<-FALSE
        
        whiche<-which(lapply(termlabels,length)!=0)
        myattr$systEqns<-names(whiche)
        myattr$ancilEqns<-"%w/o%"(namesOfEquations,myattr$systEqns)
        
        myattr$variables<-variables
        myattr$term.labels<-termlabels
        myattr$indVars<-indVars
        
        myattr$depVars<-depVars
        myattr$depFactors<-depFactors
        myattr$constraints<-constraints
        myattr$subs<-subs
        myattr$response<-1
        myattr$intercept<-intercAttr
        attributes(result)<-myattr
        names(result)<-namesOfEquations
        class(result)<-c("terms","multiple","list")
        return(result)
}

###
## Fix the deparsed tag
## 


.fixTag <- function(l){
        
        if(l$var == "1" && l$label!="none"){
                ## tag(1,z1 | state) == tag (z1|state)
                l$var <- l$label
                l$label <- "none"
                
        }
        if(l$label =="none"){
                ## tag(1+z1|state)
                vars<-.trim(unlist(strsplit(l$var,"+", fixed=TRUE)))
        }else{
                ## tag(z1,w1+w2|state)
                vars<-.trim(unlist(strsplit(l$label,"+", fixed=TRUE)))
        }
        if(length(vars) == 1){
                ## nothing to expand
                return (list(l))
        }else{
                alltgs<-list()
                for(i in 1:length(vars)){
                        if(l$label == "none")
                          alltgs[[i]] <- list(label="none",var=vars[[i]],id=l$id)
                        else
                          alltgs[[i]] <- list(label="none",var=paste(l$var,":",vars[[i]],sep=""),id=l$id)
                        
                }
        }
        return (alltgs)
        
}
