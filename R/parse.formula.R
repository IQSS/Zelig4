parse.formula<-function( formula, model,data=NULL){
        if(class(formula)[[1]]=="multiple")
          return(formula)
        nrUserOpt<-nrUserReq<-nrUserFixed<-nrUserSubreq<-0
        userOpt<-userReq<-userFixed<-userSubreq<-list()
        
        fc <- paste("describe.", model, sep = "")
        if (!exists(fc))
          stop("describe.",model," does not exsist")
        modelReq<-do.call(fc,list())
        modelReq <-modelReq$parameters
        
        checkNrReq<-function(modelNumEqn,nrUserReq,modelParsReq){
                if(length(modelNumEqn)==1){
                        if(nrUserReq != modelNumEqn)
                          stop("The parameter \"",modelParsReq,"\" requires ",modelNumEqn, " equation(s).
            You have provided ", nrUserReq, " See model doc. for more details")
                }else{
                        if(!(betweenf(nrUserReq,modelNumEqn)))
                          stop("The parameter \"",modelParsReq,"\" requires between ",modelNumEqn[[1]],"
            and ",modelNumEqn[[2]], " equation(s). You have provided ", nrUserReq, " See model doc. for more details")    
                }
        }
        
        
        checkNrOpt<-function(modelNumEqn,nrUserOpt,modelParsOpt,userOpt){
                if(!(betweenf(nrUserOpt,modelNumEqn)))
                  if(nrUserOpt < modelNumEqn[[1]]){
                          if(modelNumEqn[[1]]==1)
                            userOpt[[modelParsOpt]]<- as.formula("~1")
                          else
                            for(i in (nrUserOpt+1):modelNumEqn[[1]]){
                                    userOpt[[i]]<-as.formula("~1")
                                    names(userOpt)[[i]]<-paste(modelParsOpt,i,sep="")
                            }
                  }else
                stop("The parameter \"",modelParsOpt,"\" requires between ",modelNumEqn[[1]]," and ",modelNumEqn[[2]], " equation(s). You have provided ", nrUserOpt, " See model doc. for more details")    
                
                return(userOpt)
        }
        
        betweenf<-function(a,range){
                if (is.finite(range[[2]]))
                  return(a >= range[[1]] && a<=range[[2]])
                else
                  return(a>=range[[1]])
        }
        
        "%w/o%" <- function(x,y) x[!x %in% y]
        
        matchPars<-function(parName,userNames){
                res<-c()
                for(i in 1:length(userNames)){
                        a<-substr(userNames[[i]],nchar(parName)+1,nchar(userNames[[i]]))
                        b<-substr(userNames[[i]],1,nchar(parName))
                        if(b==parName && (!is.na(suppressWarnings(as.numeric(a))) || userNames[[i]]==parName))
                          res<-c(res,userNames[[i]])
                }
                return (res)
        }
        
        fMode<-function(b){
                if(b$depVar == TRUE && b$expVar == TRUE) return (1)
                if(b$depVar == FALSE && b$expVar == TRUE) return (2)
                if(b$depVar == FALSE && b$expVar == FALSE) return (3)
                if(b$depVar == TRUE && b$expVar == FALSE) return (4)
                stop("some error occurred ... please contact the Zelig team")
  }
        
        parsType<-lapply(modelReq,fMode)
        modelParsReq<-names(parsType[parsType==1])
        modelParsOpt<-names(parsType[parsType==2])
        modelParsFixed<-names(parsType[parsType==3])
        modelParsSubreq<-names(parsType[parsType==4])
        
        modelNrParsReq<-length(modelParsReq)
        modelNrParsOpt<-length(modelParsOpt)
        modelNrParsFixed<-length(modelParsFixed)
        modelNrParsSubreq<-length(modelParsSubreq)
        
        userNrLevels<-0
        dataNrLevels<-0
        userLevels<-c()
        
        if(class(formula)[[1]]=="formula")
          formula<-list(formula)
        
        nreqns <-length(formula)                      
        
        if(is.null(names(formula))){
                if(modelNrParsReq >1)         
                  stop("You should name the equations. The model requires more than 1 systematic component. Please see model documentation for more details")
                for (i in 1:nreqns){
                        eqni<-formula[[i]]
                        if (length(eqni)==3){                            
                                rootNames<-modelParsReq                    
                                lhs<-eqni[[2]]
                                rhs<-deparse(eqni[[3]],width.cutoff=500)
                                if(length(lhs)>1 && (lhs[[1]]=="cbind" || lhs[[1]]=="as.factor" || lhs[[1]]=="id")){
                                        if( lhs[[1]]=="cbind"){
                                        #rhs=deparse(rhs)
                                                g<- as.list(lhs)[-1]
                                                for (j in 1:length(g)){
                                                        e<-paste(g[[j]],"~",sep="")
                                                        if(rhs!="1"){
                                                                nrUserReq=nrUserReq+1
                                                                userReq[[nrUserReq]]<-as.formula(paste(e,rhs,sep=""))
                                                        }else{
                                                                nrUserSubreq=nrUserSubreq+1
                                                                userSubreq[[nrUserSubreq]]<-as.formula(paste(e,rhs,sep=""))
                                                        }
                                                }    
                                        }else{
                                                if(is.null(data))
                                                  stop("Data argument is required when you use as.factor() or id() as a dependent variable\n")
                                                if(lhs[[1]]=="as.factor"){
                                                        varname<-as.character(lhs[[2]])
                                                        userLevels<-levels(as.factor(data[[varname]]))[-1]
                                                        userNrLevels<-length(userLevels)
                                                        for (j in 1:userNrLevels){
                                                                e<-paste("id(",lhs[[2]],",\"",userLevels[[j]],"\")","~",sep="")
                                                                if(rhs!="1"){
                                                                        nrUserReq=nrUserReq+1
                                                                        userReq[[nrUserReq]]<-as.formula(paste(e,rhs,sep=""))
                                                                }else{
                                                                        nrUserSubreq=nrUserSubreq+1
                                                                        userSubreq[[nrUserSubreq]]<-as.formula(paste(e,rhs,sep=""))
                                                                }
                                                        }     
                                                }else{  
                                                        varname<-as.character(lhs[[2]])
                                                        userLevels<-c(userLevels,lhs[[3]])
                                                        userNrLevels<-length(userLevels)
                                                        levels<-levels(data[[varname]])
                                                        lhs<-deparse(lhs)
                                        #  rhs<-deparse(rhs)
                                                        e<-paste(lhs,"~",sep="")
                                                        if(rhs !="1"){
                                                                nrUserReq=nrUserReq+1
                                                                userReq[[nrUserReq]]<-as.formula(paste(e,rhs,sep=""))
                                                        }else{
                                                                nrUserSubreq<-nrUserSubreq+1
                                                                userSubreq[[nrUserSubreq]]<-as.formula(paste(e,rhs,sep=""))
                                                        }
                                                }
                                        }
                                }else{ 
                                        lhs<-deparse(lhs)
                                        #  rhs<-deparse(rhs)
                                        e<-paste(lhs,"~",sep="")
                                        if(rhs !="1"){
                                                nrUserReq=nrUserReq+1
                                                userReq[[nrUserReq]]<-as.formula(paste(e,rhs,sep=""))
                                        }else{
                                                nrUserSubreq<-nrUserSubreq+1
                                                userSubreq[[nrUserSubreq]]<-as.formula(paste(e,rhs,sep=""))
                                        }
                                }
                        }else{                            
                                rhs<-deparse(eqni[[2]])
                                if(rhs !="1"){
                                        nrUserOpt=nrUserOpt+1
                                        userOpt[[nrUserOpt]]<-as.formula(paste("~",rhs,sep=""))
                                }else{
                                        nrUserFixed=nrUserFixed+1
                                        userFixed[[nrUserFixed]]<-as.formula(paste("~",rhs,sep=""))
                                }
                        }
                }
                if (modelNrParsOpt==0){         
                        if (nrUserOpt !=0){
                                stop("the equation(s) ",userOpt," does not match model requirements!")}
                }else{                                
                        modelNumEqn<-modelReq[[modelParsOpt]]$equations
                        userOpt<-checkNrOpt(modelNumEqn,nrUserOpt,modelParsOpt,userOpt)
                        if(length(userOpt)==1)
                          names(userOpt)<-modelParsOpt
                        else
                          names(userOpt)<-paste(modelParsOpt,1:length(userOpt),sep="")
                }
                
                if(length(modelParsFixed)>0){                   
                        modelNumFixedEqns<-modelReq[[modelParsFixed]]$equations
                        for(i in 1:modelNumFixedEqns)
                          userFixed[[i]]<-as.formula("~1")
                        if(modelNumFixedEqns==1)
                          names(userFixed)<-modelParsFixed
                        else
                          names(userFixed)<-paste(modelParsFixed,1:modelNumFixedEqns,sep="")
                }
                if (modelNrParsReq==0){             
                        if (nrUserReq !=0){
                                stop("the equation(s) ",userReq," does not match model requirements!")}
                }else{
                        modelNumEqn<-modelReq[[modelParsReq]]$equations 
                        checkNrReq(modelNumEqn,nrUserReq,modelParsReq)
                        if(userNrLevels>0){
                                if(userNrLevels !=nrUserReq)
                                  stop("The number of equation for the systematic component should be equal to the number of levels -1\n")
                                names(userReq)<-userLevels
                        }else{
                                if(nrUserReq==1)
                                  names(userReq)<-modelParsReq
                                else
                                  names(userReq)<-paste(modelParsReq,1:length(userReq),sep="")
                        }
                }
                
                if (modelNrParsSubreq==0){              
                        if (nrUserSubreq !=0){
                                stop("the equation(s) ",userSubreq," does not match model requirements!")}
    }else{                                
            modelNumEqn<-modelReq[[modelParsSubreq]]$equations
            checkNrReq(modelNumEqn,nrUserSubreq,modelParsSubreq)
            if(nrUserSubreq==1)
              names(userSubreq)<-modelParsSubreq
            else
              names(userSubreq)<-paste(modelParsSubreq,1:length(userSubreq),sep="")
    }
                result<-c(userReq,userOpt,userFixed,userSubreq)
        }else{    ##user provides names for formulas
                modelPars<-names(modelReq)
                parsS<-names(sort(sapply(modelPars,nchar),decreasing=TRUE))    
                userNames<-names(formula)
                userEqnNamesByPars<-list()
                tmpUserNames<-userNames
                for (i in 1:length(parsS)){
                        userEqnNamesByPars[[parsS[[i]]]]<-matchPars(parsS[[i]],tmpUserNames)
                        tmpUserNames<-"%w/o%"(tmpUserNames,userEqnNamesByPars[[parsS[[i]]]])
                }
                tmp<-"%w/o%"(userNames,unlist(userEqnNamesByPars))
                if (length(tmp)>0)
                  stop("Ambigous equation name ","\"",tmp,"\"")
                res<-list()
                userPars<-names(userEqnNamesByPars)
                for (i in 1:length(modelPars)){ 
                        modelPar<-modelPars[[i]]                  
                        userNumEqn<-length(userEqnNamesByPars[[modelPar]])
                        modelNumEqn<-modelReq[[modelPar]]$equations
                        mode<-fMode(modelReq[[modelPar]])
                        tmplst<-formula[userEqnNamesByPars[[modelPar]]]                
                        if(modelNumEqn[[1]]==1 && modelNumEqn[[2]]==1 )
                          tmpNames<-modelPar
                        else
                          tmpNames<-paste(modelPar,1:userNumEqn,sep="")                   
                        if(mode==1){         
                                whiche<-which(lapply(formula[(userEqnNamesByPars[[modelPar]])],length)!=3)
                                if(length(whiche)!=0)
                                  stop("The equation ",formula[[names(whiche)]]," is not conform model requirements or its name is ambigous . DepVar/ExpVar is missing.\n")
                                checkNrReq(modelNumEqn,userNumEqn,modelPar)
                                whiche<-which((names(tmplst) %in% tmpNames)==FALSE)
                                if(length(whiche)!=0){
                                        warning("The name \"",names(tmplst)[whiche],"\" is ambigous. The equations of the paramter \"",modelPar,"\" are renamed\n")
                                        names(tmplst)<-tmpNames
                                }
                        }else{
                                if(mode==2){
                                        whiche<-which(lapply(formula[(userEqnNamesByPars[[modelPar]])],length)!=2)
                                        if(length(whiche)!=0)
                                          stop("The equation ",formula[names(whiche)]," is not conform model requirements or its name is ambigous A .\n")
                                        whiche<-which((names(tmplst) %in% tmpNames)==FALSE)
                                        if(length(whiche)!=0){
                                                warning("The name \"",names(tmplst)[whiche],"\" is ambigous. The equations of the paramter \"",modelPar,"\" are renamed\n")
                                                names(tmplst)<-tmpNames
                                        }       
                                        tmplst<- checkNrOpt(modelNumEqn,userNumEqn,modelPar,tmplst)
                                }else{
                                        if (mode==3){
                                                whiche<-which(tmplst !="~1")
                                                if(length(whiche)>0)
                                                  warning("You cannot specify a formula for the parameter \"",modelPar,"\" . All your equation for this parameter are set to their default value.For example your equation:\n",deparse(formula[names(whiche)]),"\n")
                                                if(userNumEqn !=modelNumEqn)
                                                  warning("The parameter \"",modelPar,"\" requires ",modelNumEqn, "equation(s). You are providing ",userNumEqn, " equation(s) for this parameter. This problem is fixed. All the equations for this parameter are set to the default value \n")
                                                
                                                tmplst<-list()
                                                if(modelNumEqn==1)
                                                  tmpname<-modelPar
                                                else
                                                  tmpname<-paste(modelPar,1:modelNumEqn,sep="")
                                                for(i in 1:modelNumEqn)
                                                  tmplst[[tmpname[[i]]]]<-as.formula("~1")
                                        }else{
                                                if(mode==4)
                                                  {
                                                          whiche<-which(lapply(formula[(userEqnNamesByPars[[modelPar]])],length)!=3)
                                                          whicha<-which(lapply(formula[(userEqnNamesByPars[[modelPar]])],FUN=function(a){if (a[[3]]=="1") return (TRUE) else return(FALSE)})==FALSE)
                                                          if(length(whiche)!=0 )
                                                            stop("The equation ",formula[names(whiche)]," is not conform model requirements or its name is ambigous . DepVar/ExpVar is missing.\n")
                                                          else{
                                                                  if (length(whicha)!=0)
                                                                    stop("The equation ",formula[names(whicha)]," is not conform model requirements or its name is ambigous . Its right hand side shoule be \"1\".\n")
                                                          }
                                                          checkNrReq(modelNumEqn, userNumEqn, modelPar)
                                                          whiche<-which((names(tmplst) %in% tmpNames)==FALSE)
                                                          if(length(whiche)!=0){
                                                                  warning("The name \"",names(tmplst)[whiche],"\" is ambigous. The equations of the paramter \"",modelPar,"\" are renamed\n")
                                                                  names(tmplst)<-tmpNames
                                                          }
                                                  }
                                        }
                                }
                        }
                        res[[modelPar]]<-tmplst
                }
                result<-c()
                for(i in 1:length(res))
                  result<-c(result,res[[i]])
                
        }
        class(result)<-c("multiple","list")
        return(result)
}
