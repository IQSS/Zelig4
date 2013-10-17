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