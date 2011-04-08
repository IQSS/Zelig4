#' Generic constructor for `mi' objects
#' 
#' @param ... an object or set objects to cast as an `mi' object
#' @param by a character-string specifying a column name
#'           in a data.frame to subset
#' @return an object of type `mi'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
mi <- function(..., by=NULL) {
  UseMethod("mi")
}


#' Construct an `mi' object from a set of data.frames
#' 
#' @param ... a set of data.frame's
#' @param by a character-string specifying a column of a data.frame
#'           to use for multiple imputation
#' @return an object of type `mi'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
mi.default <- function(..., by = NULL) {
  data.labels <- match.call(expand.dots=F)[["..."]]
  data.labels <- as.character(data.labels)

  mi(list(...), by = by)
}


#' Construct an `mi' object from another `mi' object
#'
#' This cosntructor differs from an identity operation
#' by applying the `by' parameter.
#'
#' @param m an object of type `mi'
#' @param ... ignored parameters
#' @param by a character-string specifying a calumn of the data.frames
#'           used in the `mi' object.
#' @return an object of type `mi'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
mi.mi <- function(m, ..., by=NULL) {
  if (missing(by))
    by <- m$by

  mi(m$data, by=by, data.labels=m$labels)
}


#' Construct an `mi' object from a list of data.frame's
#'
# @param lis a list of data.frames
#' @param by a character-string specifying a calumn of the data.frames
#'           used in the `mi' object.
#' @param data.labels a vector of character-strings that specify the
#'                    name of each data.frame. This parameter is currently
#'                    ignored, and is included exclusively for future
#'                    Zelig releases
#' @return an object of type `mi'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.}
mi.list <- function(lis, by=NULL, data.labels=NULL) {
  # error-catching
  if (!all(sapply(lis, is.data.frame)))
    stop("all elements passed to `mi' must be data.frame's")

  # initialize list of factors
  factors <- list()

  # build the list
  for (key in by) {
    for (dataf in lis)
      factors[[key]] <- c(factors[[key]], levels(dataf[,key]))

    # unique, bitte
    factors[[key]] <- unique(factors[[key]])
  }
  
  #if (is.null(data.labels))
  data.labels <- list(1:length(lis))

  #else if(!is.list(data.labels))
  #  data.labels <- list(dataset=data.labels)

  #p <- c(data.labels, factors)
  p <- c(list(1:length(lis)), factors)

  # get all combinations of factors
  combined <- do.call(".combine", p)

  # build object
  s <- list(data = lis,
            iter = iter(combined, by="row"),
            by = by,
            labels = data.labels
            )

 
  # assign class, and return
  class(s) <- "mi"
  s
}


#' Generic method for resetting an iterator
#'
#'
#' @param ... a list of parameters including the object
#'            to be `reset'
#' @return the reset object
reset <- function(...) UseMethod("reset")


#' Extract the next data.frame from an `mi' object
#'
#' @param m an object of type `mi'
#' @param keys.only a boolean specifying whether the return value
#'                  should exclusively be the title of the next
#'                  data.frame or the key-value pair of the title
#'                  and the actual data.frame
#' @param as.pair a boolean specifying whether the next element should
#'                be return as a key-value pair
#' @return the next data.frame, title of the data.frame, or key-value
#'         pair of title and data.frame
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
nextElem.mi <- function(m, keys.only=F, as.pair=F) {
  # assign value with error-checking
  item <- try(nextElem(m$iter), silent=T)

  # catch `stopIteration` error
  if (inherits(item, "try-error")) {
    reset(m)
    return(item)
  }

  # if we don't want a data.frame
  if (keys.only)
    return(item)

  # build parameter list
  list.item <- as.list(item)
  names(list.item) <- colnames(item)
  list.item <- list.item[-1]

  # get data.frame index
  i <- as.numeric(item[[1]])

  # if no "by" arguments were passed
  if (length(m$by) < 1)
    return(m$data[[i]])

  # cast as zframe, so we can filter
  zef <- zframe(m$data[[i]])

  # return filtered data.frame
  if (as.pair)
    list(key=item, value=zef[list.item])
  else
    zef[list.item]
}


#' Reset method for `mi' objects
#'
#' @param m 
#' @return 
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
reset.mi <- function(m) {
  assign('i', 0, env=m$iter$state)
  invisible(m)
}


#' Length method for `mi' objects
#'
#' @param m an object of type `mi'
#' @return an integer specifying the number of
#'         data.frame's that are contained within
#'         within the `mi' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
# return: number of data-frame subsets
length.mi <- function(m)
  nrow(m$iter$state$obj)
