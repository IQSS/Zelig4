#' Zelig Object Summaries
#' Compute summary data for zelig objects
#' @S3method summary zelig
#' @param object a zelig object
#' @param ... parameters forwarded to the generic summary object
#' @return the summary of the fitted model
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summary.zelig <- function (object, ...) {
  # For now, simply get the summary of the result object
  obj <- eval(object$result)

  if (isS4(obj)) {

    sigs <- findMethodSignatures('summary')
    classes <- class(obj)

    # Remove classes that do not have 'summary' methods
    intersection <- classes[ ! sigs %in% classes ]
    intersection <- na.omit(intersection)
    intersection <- as.character(intersection)

    # Summary only has one parameter, so we only consider the first one
    # This may be slightly dangerous, but it should not fail
    sig <- intersection[1]
    
    # if an attempt to get the summary fails, replace with a call to the S3
    SUMMARY <- tryCatch(getMethod('summary', sig), error = function(e) summary)

    # return
    SUMMARY(obj)
  }

  else
    # S3 objects have no problem figuring out which method to use
    summary(obj)
}

#' Multiply-Imputed Zelig Object Summaries
#' Compute summary data for a set of fitted model
#' @S3method summary zelig.MI
#' @param object a zelig.MI object
#' @param ... parameters forwarded to the fitted model
#' @return a list of summaries of the fitted models
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summary.zelig.MI <- function (obj, ...) {
  # Apply  summary.zelig to an entire list
  #Map(summary.zelig, obj$result, ...)

  warning(sprintf("Cannot produce summaries for 'zelig.MI' objects"))
}
