setx.MI <- function(z, ..., data=NULL) {
  s.x <- results <- list()

  # assign data, if NULL
  if (is.null(data))
    data <- z$data

  # create iterators
  result.iter <- iter(z$result)
  data.iter <- z$mi

  repeat {
    # get next items from iterators
    result.item <- try(nextElem(result.iter), silent=T)
    data.item <- try(nextElem(data.iter), silent=T)

    # if end of list
    if (inherits(result.item, "try-error") &&
        inherits(data.item, "try-error"))
      break

    else if (inherits(result.item, "try-error") ||
             inherits(data.item, "try-error")) {
      warning("number of data-sets and fitted models do not match.")
      break
    }

    # create kin object
    kin <- zelig.kin(z, result.item, data=data.item)

    # call the correct setx function
    result <- setx(kin, data=data.item, ...)

    # add results to the list
    results[['']] <- result
  }

  s.x.mi <- list(s.x=results, by=z$by, levels=z$levels)

  class(s.x.mi) <- c("MI", "setx.mi", "setx")
  s.x.mi
}
