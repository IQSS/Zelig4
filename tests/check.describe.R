###
## check the correctness of the describe.mymodel functions
##


check.describe<-function(mymodel){
        firstLvlNames<-c("category","description","package","parameters", "year", "authors")
        fn <- paste("describe.", mymodel, sep = "")
        
        if (!exists(fn,env=asNamespace("Zelig")))
          stop("The function describe.",mymodel," does not exist")
        z<-do.call(fn,list(),env=asNamespace("Zelig"))
        
        ##any extra name in the list??
        whiche<-which(!(names(z) %in% firstLvlNames))
        if (length(whiche)!=0){
                tmpstr<-names(z)[[whiche[[1]]]]
                if(length(whiche)>1)
                  for(i in 2:length(whiche))
                    tmpstr<-paste(tmpstr,names(z)[[whiche[[i]]]],sep=",")
                stop ("Unknown names in your list: ",tmpstr)
        }
        errmsg<-" is missing. It's required ..."
        if(is.null(z$category))
          stop("\"category\"", errmsg)
        else
          if(!(z$category %in% names(Zelig:::categories())))
            stop("unknown category \"",z$category, "\"")
        if(is.null(z$parameters)) stop("\"parameters\"",errmsg)
        
        for (i in length(z$parameters)){
                eqns<-z$parameters[[i]]$equations
                if(is.null(eqns))  stop("\"equations\"",errmsg)
                if(length(eqns)!=2) stop("equations must be an vector of length 2")
                if(!(eqns[[2]] <= 999 || !(is.finite(eqns[[2]]))) ) stop("The maximum number of equations for each paramter should be <=999 or \"Inf\"..")
                
                tags<-z$parameters[[i]]$tagsAllowed
                if (is.null(tags)) stop ("\"tagsAllowed\"",errmsg)
                if(!is.logical(tags)) stop("\"tagsAllowed\" must have a logical value (\"TRUE\" or \"FALSE\")")
                
                dep<-z$parameters[[i]]$depVar
                if (is.null(dep)) stop ("\"depVar\"",errmsg)
                if(!is.logical(dep)) stop("\"depVar\" must have a logical value (\"TRUE\" or \"FALSE\")")
                
                exp<-z$parameters[[i]]$expVar
                if (is.null(exp)) stop ("\"expVar\"",errmsg)
                if(!is.logical(exp)) stop("\"expVar\" must have a logical value (\"TRUE\" or \"FALSE\")")
                
                tags<-z$parameters[[i]]$tagsAllowed
                if (is.null(tags)) stop ("\"tagsAllowed\"",errmsg)
                if(!is.logical(tags)) stop("\"tagsAllowed\" must have a logical value (\"TRUE\" or \"FALSE\")")
        }
        
        cat("\"",fn, "\" passed the test\n")
        return (fn)
}

###
## get all the models name
##

zeligListModels<-function() {
        res = ls(envir=asNamespace("Zelig"),pattern="^zelig2")
        res
        ## describe function for this models does not exists
        ## just for testing purpuses i'm excluding them. <FIXME> urgent
        ##nonexist <- c("mloglm")
        ##setdiff(sub("zelig2","", tmp),nonexist)
        
}

###
## check if all models have proper describe function
##

checkDescribe <- function(){
        loosers <- c()
        listOfModels <- zeligListModels()
        for (mymodel in listOfModels){
                mymodel<-sub("^zelig2","",mymodel)
                tmp<- try(check.describe(mymodel), silent=TRUE)
                if (inherits(tmp,"try-error")) {
                        loosers <- c(loosers,tmp)
                }
        }
        return (loosers)
}

## when source, this is the function wich will be executed
res <- checkDescribe()
if (length(res)>0) {
        print(res)
        stop("the above models have no mymodel.describe() function or that function did not paseed the test")
} else
print ("describe.* functions OK ...")
