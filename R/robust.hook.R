#' @export
robust.gee.hook <- function(obj, Zall, Call, robust, ...) {
  
  # Assume robust, if nothing is specified
  if (missing(robust) || is.null(robust))
    robust <- TRUE

  # Invalid robust parameters should stop program
  if (!is.logical(robust))
    stop("robust must be a logical (TRUE or FALSE)")

  if (robust)
    class(obj) <- c("gee.robust", class(obj))

  else
    class(obj) <- c("gee.naive", class(obj))

  #
  obj
}
