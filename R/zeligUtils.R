#' Utility Functions for Zelig Package and Add-on Modules.


#' This takes model output from a zelig call, and calculates a hypothesis
#'    test on a coefficient of interest.
#' @param model Output of a Zelig model.
#' @param null Null value of the hypothesis test.
#' @param var Variable name on whose coefficient to conduct hypothesis test.
#' @param onetail If TRUE, calculates a one-tail test, otherwise two-tails.
#' @author Vito D'Orazio
#' @export

spectest <- function(model, null=0, var, onetail=FALSE) {
    
    z.coef <- model$result$coefficients
    z.df <- model$result$df.residual
    z.se <- sqrt(diag(vcov(model)))
    
    var.i <- which(names(z.coef)==var)
    if(onetail){
        tail.multiplier<-1
    }else{
        tail.multiplier<-2
    }
    pval <- (1 - pt(abs(z.coef[var.i]-null)/z.se[var.i],z.df) )*tail.multiplier
    
    xseq <- seq(from=min(model$data[var.i]), to=max(model$data[var.i]), length=25)
    
    ## passing x0 using var
    ## or passing setx a parameter stored in function spectest's input
    ## or can't believe this works
    woah <- substitute(list(obj==model, var==xseq), list(var=as.name(var)))
    woah <- deparse(woah)
    woah <- gsub("==", "=", woah)
    x.out <- do.call(setx, eval(parse(text=woah)))
    
    s.out <- sim(model, x=x.out)
    plot.ci(s.out)
    
    rm(xseq, envir=.GlobalEnv)
    return(pval)
    
}



# a function whose parameters are a Zelig model and a dataset of counterfactual examples.  It returns the counterfactual dataset where the DV is the mean expected value and inHull is a dummy for each observation


zwhat <- function(model, cfs) {
    
    for(i in 1:nrow(cfs)) {
        x.out <- setx(model, data=cfs[i,], fn=NULL)
        s.out <- sim(model, x = x.out)
        y.preds[i] <- mean(s.out$qi$ev1)
    }
    
    dv.name <- as.name(all.vars(update(model$formula, . ~ 1)))
    dv.index <- which(colnames(cfs)==dv.name)
    cfs[,dv.index] <- y.preds
    colnames(cfs)[dv.index] <- "yhat"
    
    my.what <- whatif(data=model$data, cfact=cfs)
    inHull <- my.what$in.hull
    out <- cbind(cfs, inHull)
    return(out)
    
}


# Construct Cubic Smoothing Spline Basis for Defined Variable

zeligBuildSpline<-function(formula, k, data){
    
    if(!is.data.frame(data)){
        data<-as.data.frame(data)
    }
    
    # if formula is of type formula, then deparse
    if(is(formula,"formula")){
        formula<-deparse(formula)
    } # ...maybe add extra conditions.

    pos<-regexpr("s\\(.+\\)",formula)
    
    if(pos[1]>0){
    
        var<-substr(formula,pos[1]+2,pos[1]+attr(pos,"match.length")-2)  # find first variable inside "s(.)" notation
        var<-sub("^\\s+", "", var) #trim leading whitespace
        var<-sub("\\s+$", "", var)   #trim trailing whitespace
        formula<- sub("s\\(.+\\)", var, formula)  #replace "s(x)" with "x"
    
    
        kseq<-seq(from=min(data[,var],na.rm=TRUE),to=max(data[,var],na.rm=TRUE),length=k+2)
        kseq<-kseq[2:(k+1)]
        copy<-data[,var]
        
        # add squared and cubic terms
        tempname<-paste(var, "squared",sep="")
        formulaAddition<-tempname
        tempvar <- copy^2
        data<-cbind(data,tempvar)
        names(data)<-c(names(data[,1:(ncol(data)-1)]),tempname)
        
        tempname<-paste(var, "cubed",sep="")
        formulaAddition<-c(formulaAddition, tempname)
        tempvar <- copy^3
        data<-cbind(data,tempvar)
        names(data)<-c(names(data[,1:(ncol(data)-1)]),tempname)
        
        # add knot points
        for(i in 1:k){
            tempname<-paste(var,"knot",i,sep="")
            formulaAddition<-c(formulaAddition, tempname)
            tempvar <- 0 + as.numeric(copy>kseq[i]) * ((copy - kseq[i])^3)
            data<-cbind(data,tempvar)
            names(data)<-c(names(data[,1:(ncol(data)-1)]),tempname)
        }

        formulaAddition<-paste(formulaAddition, collapse=" + ")
        formula<-as.formula( paste(formula, formulaAddition, sep=" + ") )
        
    }
    
    # Currently does nothing, including no error message, if s() not found.  Consider adding warning message.
    return(list(formula=formula,data=data))
    
}





