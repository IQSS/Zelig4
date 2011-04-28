#' Extract a Value from a \code{zelig} Fitted Model
#' @note This function is primarily used by Zelig developers within \code{qi}
#'   functions
#' @param obj a \code{zelig} object
#' @param key a character-string specifying the which value to extract from
#'   the fitted model object  
#' @param ... subsequent values to extract from the fitted model object
#' @return values of the specified keys
#' @export
#' @author Matt Owen \emph{mowen@@iq.harvard.edu}
GetSlot.zelig <- function(obj, key, ...) {
  # expand dots
  dots <- list(...)

  # error-catching
  if (!all(sapply(dots, is.character)))
    stop("all dot parameters must be characters")

  # get result of zelig object
  obj <- obj$result
  #
  res <- obj[[key]]

  for (key in dots) {
    # 
    res <- try(res[[key]], silent=T)

    # if it doesn't exist, then NULL
    if (inherits(res, "try-error"))
      return(NULL)
  }

  # return
  res
}
