param.MI <- function(z, num, bootstrap) {
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
    res[['']] <- as.parameters(param(kin, num, bootstrap), num=num)
  }

  param.obj <- list(results=res)
  class(param.obj) <- c("parameters.MI", "parameters")
  param.obj
}
