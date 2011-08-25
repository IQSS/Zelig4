#' Bundle Multiply Imputed Data Sets as a List
#' 
#' The mi constructor bundles several data-frames with identical 
#' columns into a single object. This allows for several analyses
#' to be executed sequentially.
#' @param obj an object
#' @param ... an object or set objects to cast as an 'mi' object
#' @param by a character-string specifying a column name
#'           in a data-frame to subset
#' @return an object of type 'mi'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @seealso The full Zelig manual is available at
#'   \url{http://gking.harvard.edu/zelig}
#'
#' @examples
#' data(immi1, immi2, immi3, immi4, immi5)
#' mi(immi1, immi2, immi3, immi4, immi5)
mi <- function(..., by=NULL) {

  if (inherits(..1, 'mi') && is.null(by))
    return(..1)

  else if (inherits(..1, 'mi') && !is.null(by)) {
    warning('the by parameter is being ignored')
    return(..1)
  }

  #
  frames <- list(...)

  # Substitute all objects in "..."
  call.object <- substitute(list(...))
  Names <- as.character((call.object[-1]))

  # ensure that all frames are named
  names(frames) <- name.frames(frames, Names)


  # retrieve factors
  to.combine <- append(
                       list(names(frames)),
                       retrieve.factors(frames, by)
                       )


  # this object gives us all the necessary information to appropriately subset
  # a data.frame
  datasets <- do.call('.combine', to.combine)

  message("Ended expectedly")
  q()


  # return mi object
  self <- list(
               by = by,
               iter = 0,
               frames = frames,
               list = datasets
               )
  class(self) <- "mi"
  self
}


# 
#
name.frames <- function (frames, frame.names) {
  # Determine titles of data.frames
  frame.labels <- if (is.null(names(frames)))
    rep('', length(frames))
  else
    names(frames)

  unnamed.frames <- which(frame.labels == '')

  frame.labels[unnamed.frames] <- frame.names[unnamed.frames]
  frame.labels
}

#' Retrieve a list of all the factors in the specified column
retrieve.factors <- function (frames, by=NULL) {
  factor.list <- list()

  for (col in by) {
    message(" >> ", col)
    for (frame in frames) {
      # ignore data.frame if this column does not exist
      if (! col %in% colnames(frame)) {
        message("col = ", col)
        next
      }

      vals <- factor.list[[col]]

      new.vals <- if (is.factor(frame[, col]))
        levels(frame[, col])
      else
        unique(frame[, col])

      vals <- unique(c(vals, new.vals))
      factor.list[[col]] <- vals
    }
  }

  factor.list
}

#' Construct an 'mi' object from a set of data-frames
#' 
#' @param obj the first data-frame
#' @param ... additional data-frames
#' @param by a character-string specifying a column of a data.frame
#'           to use for multiple imputation
#' @return an object of type `mi'
#' @S3method mi default
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
mi.default <- function(obj, ..., by = NULL) {
  data.labels <- match.call(expand.dots=F)[["..."]]
  data.labels <- as.character(data.labels)

  lis <- append(list(obj), list(...))

  mi(lis, by = by)
}


#' Construct an 'mi' object from another 'mi' object
#' This cosntructor differs from an identity operation
#' by applying the 'by' parameter.
#'
#' @S3method mi mi
#' @export
#' @param obj an object of type 'mi'
#' @param ... ignored parameters
#' @param by a character-string specifying a calumn of the data.frames
#'           used in the 'mi' object.
#' @return an object of type 'mi'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
mi.mi <- function(obj, ..., by=NULL) {
  if (missing(by))
    by <- obj$by

  mi(obj$data, by=by, data.labels=obj$labels)
}


#' Construct an 'mi' object from a list of data.frame's
#'
#' This is the main constructor; all other constructors - in one fashion
#' or another - call this method to construct the final list of data-frames.
#' Care should be taken when developing code for this method, as it can
#' cause a myriad of additional problems.
#'
#' @S3method mi list
#' @export
#' @param obj a list of data.frames
#' @param ... additional parameters
#' @param by a character-string specifying a calumn of the data.frames
#'           used in the `mi' object.
#' @param data.labels a vector of character-strings that specify the
#'                    name of each data.frame. This parameter is currently
#'                    ignored, and is included exclusively for future
#'                    Zelig releases
#' @return an object of type `mi'
#' @author Matt Owen \email{mowen@@iq.harvard.}
mi.list <- function(obj, ..., by=NULL, data.labels=NULL) {
  # error-catching
  if (!all(sapply(obj, is.data.frame)))
    stop("all elements passed to `mi' must be data.frame's")

  # initialize list of factors
  factors <- list()

  # build the list
  for (key in by) {
    for (dataf in obj)
      factors[[key]] <- c(factors[[key]], levels(dataf[,key]))

    # unique, bitte
    factors[[key]] <- unique(factors[[key]])
  }
  
  #if (is.null(data.labels))
  data.labels <- list(1:length(obj))

  #else if(!is.list(data.labels))
  #  data.labels <- list(dataset=data.labels)

  #p <- c(data.labels, factors)
  p <- c(list(1:length(obj)), factors)

  # get all combinations of factors
  combined <- do.call(".combine", p)

  # build object
  s <- list(data = obj,
            iter = combined,
            by = by,
            labels = data.labels
            )

 
  # assign class, and return
  class(s) <- "mi"
  s
}


#' Generic method for resetting an iterator
#' Resets the incrementor of an iterator
#' @note This method is not exported; it is used internally by Zelig, but is not
#'   registered for use in the Global namespace or search path. It is unclear
#'   whether the  'reset' method is a desirable feature for other packages
#' @param obj the iterator-like object to be reset
#' @param ... a list of parameters of other parameters
#' @return the original object, except with its internatl
#'   iterator pointing to the first element of its list
#' @export
reset <- function(obj, ...)
  UseMethod("reset")


#' Extract the Next Data-frame from an 'mi' Object
#' Produces the next data-frame from the iterator list. If
#' the 'keys.only' parameter  is set to TRUE (defaults FALSE), then
#' exclusively the human-readable label is listed. If the 'as.pair'
#' parametet is set to TRUE (defaults FALSE), then a list containing
#' two entries ("key" and "value") are displayed. "key" contains the
#' label of the data-frame as a character string, and "value" contains
#' the actual subsetted data-frame.
#' @usage \method{nextElem}{mi}(obj, \dots, keys.only=F, as.pair=F)
#' @S3method nextElem mi
#' @param obj an object of type 'mi'
#' @param ... ignored parameters
#' @param keys.only a boolean specifying whether the return value
#'   should exclusively be the title of the next data.frame or
#'   the key-value pair of the title and the actual data.frame
#' @param as.pair a boolean specifying whether the next element should
#'                be return as a key-value pair
#' @return the next data.frame, title of the data.frame, or key-value
#'         pair of title and data.frame
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
nextElem.mi <- function(obj, ..., keys.only=F, as.pair=F) {
  # assign value with error-checking
  item <- try(nextElem(obj$iter), silent=TRUE)

  # catch 'stopIteration' error
  if (inherits(item, "try-error")) {
    reset(obj)
    return(item)
  }


  # the following is kludge that is useful for formatting
  # mi objects that have multiple data-frames
  # ...
  # that is, it specifies the leading column as being 
  # title "data #", so that we have an explicit title
  # for an otherwise untitled label
  pretty.item <- item[1, ]
  names <- names(pretty.item)
  
  if (names[1] == '')
    names[1] <- 'data'

  names(pretty.item) <- names

  # if we don't want a data.frame
  if (keys.only)
    return(pretty.item)

  # build parameter list
  list.item <- as.list(item)
  names(list.item) <- colnames(item)
  list.item <- list.item[-1]

  # get data.frame index
  i <- as.numeric(item[[1]])

  # if no "by" arguments were passed
  if (length(obj$by) < 1)
    return(obj$data[[i]])

  # cast as zframe, so we can filter
  zef <- zframe(obj$data[[i]])

  # return filtered data.frame
  if (as.pair)
    list(key=pretty.item, value=zef[list.item])
  else
    zef[list.item]
}


#' Reset method for \code{mi} objects
#' @usage \method{reset}{mi}(obj, ...)
#' @S3method reset mi
#' @param obj an \code{mi} object 
#' @param ... ignored parameters
#' @return the same object with the iterator reset
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
reset.mi <- function(obj, ...) {
  assign('i', 0, env=obj$iter$state)
  invisible(obj)
}


#' Get List of Labels from MI object
#' @param obj an mi object
#' @param ... ignored (for now) parameters
#' @return a vector of character-strings detailing - in human readable format -
#'   the titles of each 
get.mi.labels <- function(obj, ...) {
  # get old position of iterator
  old.pos <- get('i', env=obj$iter$state)

  # reset iterater
  reset(obj)

  # NULL 
  mi.labels <- c()

  repeat {
    item <- try(nextElem(obj, keys.only=TRUE))

    if (inherits(item, 'try-error'))
      break

    smoosh <- label.from.key(item)

    mi.labels <- c(mi.labels, smoosh)
  }

  # restore old iterator position
  assign('i', old.pos, env=obj$iter$state)

  # return the list of labels
  mi.labels
}


#' Get Labels from MI Key
#'
#' This produced a human-readable label from a named character vector. This is
#' primarily used by the zelig function to give reasonable names to simulated
#' quantities of interest. That is, we can differentiate several imputed
#' data-frames from one another by which key we read.
#'
#' @note This method is not exported, and is exclusively used internetally by
#'   Zelig
#' @param key a vectory of character-strings
#' @return a character-string
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
label.from.key <- function(key) {
  item <- key

  if (names(item)[1] == 'data') {
    head.sep <- ' '
    join.sep <- ': '
  }
  else {
    head.sep <- '='
    join.sep <- ', '
  }

  head <- paste(names(item)[1], item[1], sep=head.sep)
  tail <- paste(names(item)[-1], item[-1], sep='=', collapse=', ')

  paste(c(head, tail), collapse=join.sep)
}


#' Length Method for 'mi' Objects
#' Compute the length of the number data-frames to be expected.
#' @note This function is primarily used internally by Zelig.
#' @usage \method{length}{mi}(x)
#' @S3method length mi
#' @note This function is exported, since it could be a conceivably useful
#'   feature for end-users.
#' @param x an object of type 'mi'
#' @return an integer specifying the number of subsetted data-frame's that are
#'   contained within within the 'mi' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
length.mi <- function(x)
  nrow(x$iter$state$obj)
