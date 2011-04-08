#' Set explanatory variables for multiply-imputed data
#'
#' @S3method setx default
#'
#' @param obj a `zelig' object
#' @param fn a list of key-value pairs specifying which function apply to
#'           columns of the keys data-types
#' @param data a data.frame
#' @param cond ignored
#' @param ... parameters specifying what to explicitly set each column as. This
#'            is used to produce counterfactuals
#' @return a `setx' object
#' @export
#' @author Matt Owen, Kosuke Imai, and Olivia Lau \email{mowen@@iq.harvard.edu}
setx.MI <- function(obj, ..., data=NULL) {
  s.x <- results <- list()

  # assign data, if NULL
  if (is.null(data))
    data <- obj$data

  # create iterators
  result.iter <- iter(obj$result)
  data.iter <- obj$mi

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
    kin <- zelig.kin(obj, result.item, data=data.item)

    # call the correct setx function
    result <- setx(kin, data=data.item, ...)

    # add results to the list
    results[['']] <- result
  }

  s.x.mi <- list(s.x=results, by=obj$by, levels=obj$levels)

  class(s.x.mi) <- c("MI", "setx.mi", "setx")
  s.x.mi
}
