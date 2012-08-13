#' Print a Summary of a Set of Pooled Simulated Interests
#'
#' Prints the summary information from a set of pooled simulated interests. This
#' method assumes that quantities of interest are kept in a data type which can
#' be used with ``rbind''.
#' @usage \method{print}{summary.pooled.sim}(x, ...)
#' @S3method print summary.pooled.sim
#' @param x a ``summary.pooled.sim'' object, containing summarized information
#' about simulated quantities of interest
#' @param ... Optional parameters that will be passed onward to ``print.matrix''
#' (the matrix printing function)
#' @return a ``summary.pooled.sim'' object storing the quantities of interest
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.summary.pooled.sim <- function (x, ...) {
  # los labels... kinda like spanish for "the labels"
  # labels is function name in base, so we needed a name that said "labels,"
  # without using "labels". You know?
  los.labels <- x$labels
  los.titles <- x$titles

  # Pooled summarized data
  for (title in los.titles) {

    # This will implicity become a matrix
    m <- NULL

    for (label in los.labels)
      m <- rbind(m, x$stats[[label]][[title]])

    rownames(m) <- paste("[", los.labels, "]", sep="")

    cat(title, "\n")
    print(m)
    cat("\n\n")
  }
}
