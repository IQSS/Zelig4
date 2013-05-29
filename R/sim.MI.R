#' Simulate Multiply Imputed Data
#' @usage \method{sim}{MI}(obj, x=NULL, x1=NULL, y=NULL, num=1000, ...)
#' @S3method sim MI
#' @param obj a 'zelig.MI' object containing several fits for two or more 
#'   subsetted data-frames
#' @param x a 'setx.mi' object containing explanatory variables for each
#'   fitted model
#' @param x1 a 'setx.mi' object containing explanatory variables for each
#'   fitted model
#' @param y this feature is currently unimplemented
#' @param num an integer specifying the number of simulations to compute
#' @param ... ignored parameters
#' @return a 'sim.MI' with simulated quantities of interest for each fitted
#'   contained by 'obj'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @seealso \link{sim}
sim.MI <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, ...) {

  sim.results <- list()
  m<-length(obj)
  mi.num<-ceiling(num/m)

  for (key in names(obj)) {
    object <- obj[[key]]
    new.x <- x[[key]]
    new.x1 <- x1[[key]]
    new.y <- y[[key]]

    sim.results[[key]] <- sim(object, x=new.x, x1=new.x1, y=new.y, num=mi.num)
  }

  model <- get('model', attr(obj, 'state'))

  class(sim.results) <- c(
                          'MI.sim',
                          paste(model, "mi-sim", sep="-"),
                          paste(model, "mi.sim", sep=".")
                          )

  sim.results
}

