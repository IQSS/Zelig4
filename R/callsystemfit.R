#' Fit Multi-stage Linear Models
#' @param formula a ``formula'' object
#' @param data a ``data.frame'' object
#' @param method a character-string
#' @param inst instrument parameters
#' @param ... other parameters passed to systemfit
#' @export
callsystemfit<-function(formula,data,method,inst=NULL,...) {
        out <- systemfit(data=data,formula=formula,method=method,inst=inst,...)
        class(formula) <- c("multiple","list")
        t <- terms(formula)
        attr(out,"terms") <- t
        class(out) <- c("multiple",class(out))
        return (out)
}
