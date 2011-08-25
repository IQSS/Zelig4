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

  print(datasets)
  q()

  # return mi object
  self <- list(
               by     = by,
               frames = frames,
               list   = datasets
               )
  class(self) <- "mi"
  self
}


#' Replace Unnamed Indices with a Default Value
#'
#' ...
#' @note This method is used by Zelig to organize collections of data.frame's
#'   for use with multiple imputation. That is, this function correctly labels
#'   lists of data.frame's submitted to the code{zelig} function.
#' @param frames
#' @param frame.names
#' @return 
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
name.frames <- function (frames, frame.names) {
  # Specify the names of each index as "" if doesn't exist
  frame.labels <- if (is.null(names(frames)))
    rep('', length(frames))
  else
    names(frames)

  # Get the index of unnamed indices
  unnamed.frames <- which(frame.labels == '')

  # Name all the unnamed frames their call-name
  frame.labels[unnamed.frames] <- frame.names[unnamed.frames]
  frame.labels
}

#' Retrieve a list of all the factors in the specified column
retrieve.factors <- function (frames, by=NULL) {
  factor.list <- list()

  for (col in by) {
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
