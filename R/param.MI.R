#' param method for multiply-imputed data
#'
#' @S3method param MI
#' 
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @return a list to be cast as a `parameters' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.MI <- function(z, num, ...) {
  # init
  res <- list()
  zelig.iter <- iter(z$result)

  # loop over zelig results
  repeat {
    # get item
    zelig.item <- try(nextElem(zelig.iter), silent=T)

    # break loop if end-of-list
    if (inherits(zelig.item, "try-error"))
      break

    # create individual fitted model
    kin <- zelig.kin(z, zelig.item)

    # append to result list
    res[['']] <- as.parameters(param(kin, num, ...), num=num)
  }

  param.obj <- list(results=res)
  class(param.obj) <- c("parameters.MI", "parameters")
  param.obj
}
