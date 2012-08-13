#' Return a Summary of a Set of Pooled Simulated Interests
#'
#' Returns the summary information from a set of pooled simulated interests.
#' The object returned contains the slots ``labels'', a character-vector
#' specifying the labels (explanatory variable titles) of the qi's, ``titles'',
#' a character vector specifying the names of the quantities of interest, and
#" ``stats'', a list containing quantities of interests.
#' @usage \method{summary}{pooled.sim}(object, ...)
#' @S3method summary pooled.sim
#' @param object a ``pooled.sim'' object, containing information about
#' simulated quantities of interest
#' @param ... Ignored parameters
#' @return a ``summary.pooled.sim'' object storing the replicated quantities of
#' interest
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
