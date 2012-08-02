#' Return a Summary of a Set of Pooled Simulated Interests
#'
#' Returns a 
#' @S3method summary pooled.sim
#' @param object a ``pooled.sim'' object, containing information about
#' simulated quantities of interest
#' @param ... 1
#' @return a ``summary.pooled.sim'' object storing the replicated quantities of interest
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summary.pooled.sim <- function (object, ...) {
  model <- list()
  stats <- list()
  titles <- list()
  original <- list()
  call <- list()
  x <- list()
  x1 <- list()

  #
  for (key in names(object)) {
    o <- object[[key]]

    stats[[key]] <- o$stats
    titles[[key]] <- o$titles
  }

  s <- list(
            labels = names(object),
            titles = names(object[[1]]$stats),
            stats = stats
            )

  class(s) <- "summary.pooled.sim"

  s
}
