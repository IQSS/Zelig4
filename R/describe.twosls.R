#' Describe a `twosls' model to Zelig
#' @usage \method{describe}{twosls}(...)
#' @S3method describe twosls
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.twosls<-function(...){
category <- "continuous"
description  <- "Two Stage Least Squares"
authors <- c("Ferdinand Alimadhi","Ying Lu", "Elena Villalon")
year <- 2007
package <-list(	name 	="systemfit",
		version	="0.8"
		)
parameters<-list(mu="mu", inst="inst")
parameters$mu<-list(equations=c(2,Inf),
			tagsAllowed=TRUE,
			depVar=TRUE,
			expVar=TRUE)
			
parameters$inst<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=FALSE,
			expVar=TRUE)
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
