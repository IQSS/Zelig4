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