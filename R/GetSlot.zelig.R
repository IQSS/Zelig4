#' Extract a slot from a `zelig' object
#'
#' @param z an object of type `zelig'
#' @param slot a character-string specifying the slot to 
#'             extract from the fitted model object
#' @param ... subsequent slots to extract from the fitted
#'            model object
#' 
#' @return contents of the specified slots
#' @export
#' @author Matt Owen \emph{mowen@@iq.harvard.edu}
GetSlot.zelig <- function(z, slot, ...) {
  # expand dots
  dots <- list(...)

  # error-catching
  if (!all(sapply(dots, is.character)))
    stop("all dot parameters must be characters")

  # get result of zelig object
  obj <- z$result
  #
  res <- obj[[slot]]

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
