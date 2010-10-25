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
