#' Count the Depth of a List Object
#'
#' This function recursively computes the depth of a list object. That is, it
#' determines how many layers or levels exist within the object.
#' @note This function is used internally by Zelig.
#' @param obj a vector or list object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
list.depth <- function (obj) {

  # Stop-recursing conditions

  if (length(obj) == 0)
    return(0)

  else if (is.atomic(obj))
    # Atomic vectors can only have one level
    return(1)

  else if (!is.list(obj))
    # If the object is not a list, then we have the option whether to compute
    # the depth of its elements.
    return(1)

  # Produce a list of integers, specifying the depth of each element
  results <- Map(list.depth, obj)

  # Ensure that the result is a non-list
  results <- unlist(results)

  # Find the maximum, ensuring that the value is neither negative nor -Inf
  max.depth <- max(results, 0)

  # Add one for the level that we are on
  1 + max.depth
}
