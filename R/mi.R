#' Bundle Data-sets for Multiple Imputation
#' 
#' This object prepares data-sets for processing with multiple imputation.
#' @note This function is largely identical to simply creating a list object,
#'   with the exception that any unnamed data-sets are automatically labeled
#'   via the \code{substitute} function
#' @param ... a set of \code{data.frame}'s
#' @return an \code{almost.mi} object, which contains the important internals
#'   of a valid, useful \code{mi} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
mi <- function (...) {
  frames <- list(...)

  # Substitute all objects in "..."
  call.object <- substitute(list(...))
  
  # Remove 'list' from the head of the call object,
  # and convert to character-strings
  Names <- as.character(call.object[-1])

  # Ensure that all frames are named
  names(frames) <- name.frames(frames, Names)

  # Check for invalid entries
  for (title in names(frames)) {
    val <- frames[[title]]

    if (!is.data.frame(val))
      stop("the entry ", title, " is not a valid data.frame object")
  }

  class(frames) <- "almost.mi"
  frames
}

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
multi.dataset <- function (obj, by=NULL) {
  UseMethod("multi.dataset")
}

multi.dataset.amelia <- function (obj, by=NULL) {
  #
  imputations <- obj$imputations

  # 
  class(imputations) <- NULL

  # Create generic labels
  names(imputations) <- paste('imputation', 1:length(imputations), sep = "")


}

#
#
#
multi.dataset.default <- function(obj, by=NULL) {

  if (is.data.frame(obj))
    obj <- eval(call("mi", substitute(obj)))

  # Retrieve factors
  to.combine <- append(
                       list(names(obj)),
                       retrieve.factors(obj, by)
                       )

  # This object gives us all the necessary information to appropriately subset
  # a data.frame
  datasets <- do.call('combine', to.combine)

  # Create an iterator-like object
  state <- new.env()
  assign('iter', 1, state)
  assign('keys', datasets, state)
  assign('frames', obj, state)

  # Return mi object
  self <- list(
               by     = by,
               state  = state,
               list   = datasets
               )
  class(self) <- "multi.dataset"
  self
}

#' Get Details of Next \code{data.frame} from an \code{mi} Object
#'
#' Produce the label to be used for the next \code{data.frame} stored within 
#' the \code{mi} object. This function replaces the necessity for Zelig to 
#' depend on the \code{iterators} package.
#' @note This method is intended for internal use by Zelig.
#' @param obj an \code{mi} object
#' @return a character-vector specifying the next representation of a label or
#'   NULL (if all labels have been produced)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
NextLabel <- function (obj) {
  i <- get('iter', envir=obj$state)
  assign('iter', i+1, envir=obj$state)
  tryCatch(obj$list[i, ], error=function (e) NULL)
}

#' Get Next \code{data.frame} from an \code{mi} Object
#'
#' Generate the next \code{data.frame}. This function removes the necessity for
#' Zelig to depend on the \code{iterators} package.
#' @note This method is intended for internal use by Zelig.
#' @param obj an \code{mi} object
#' @param as.pair a boolean specifying whether to return the \code{data.frame}
#'   as a list pairing the label and the data object
#' @return the next \code{data.frame} or NULL (if all \code{data.frames} have
#'   been generated). If \code{as.pair} is set to TRUE, then a list will be 
#'   returned, specifying the \code{data.frame}'s label as ``label'' and the
#'   \code{data.frame} as ``data''
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
NextFrame <- function (obj, as.pair = FALSE) {
  key <- NextLabel(obj)

  if (is.null(key))
    return(NULL)

  Name <- key[1]
  Constraints <- key[-1]
  Data <- get('frames', obj$state)[[Name]]

  for (col in names(Constraints)) {
    hits <- which(Data[, col] == Constraints[col])
    Data <- Data[hits, ]
  }

  if (as.pair)
    list(label = key, data = Data)
  else
    Data
}

#' Reset Counter for \code{mi} Object
#'
#' This function sets the next frame of the iterator to be its first value.
#' @note This method is used for its side-effect. Its return-value is of almost
#'   no significance.
#' @param obj an \code{mi} object
#' @return the value 1 (invisibly)
#' @export
Reset <- function (obj) {
  assign('iter', 1, obj$state)
}

#' Replace Unnamed Indices with a Default Value
#'
#' @note This method is used by Zelig to organize collections of data.frame's
#'   for use with multiple imputation. That is, this function correctly labels
#'   lists of data.frame's submitted to the code{zelig} function.
#' @param frames q list of \code{data.frame}'s
#' @param frame.names a character vecotory specifying the appropriate names to
#'   label each index of \code{frames} if no label is already set. If every
#'   \code{data.frame} already has a label, then the \code{frame.names}
#'   parameter is essentially ignored
#' @return a character-string specifying the appropriate names for the 
#'   \code{data.frame}'s
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
#' 
#' @param frames a list of \code{data.frame} objects
#' @param by a character-vector specifying columns to search for factors within
#' @return a list with entries corresponding to the value of the \code{by}
#'   parameter. The value of each index is unique values of the
#'   \code{data.frame}'s columns.
#' @export
retrieve.factors <- function (frames, by=NULL) {
  factor.list <- list()

  for (col in by) {
    for (frame in frames) {
      # Ignore data.frame if this column does not exist
      if (! col %in% colnames(frame)) {
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
length.multi.dataset <- function(x) {
  nrow(x$list)
}

#' Get Labels from \code{mi} Object
#' 
#' Find a suitable set of labels for titling each individual \code{mi} object.
#' @note This function is primarily used internally by Zelig to keep track of
#'   the \code{data.frame}'s produced by \code{mi} objects.
#' @S3method labels mi
#' @usage \method{labels}{mi}(object, ...)
#' @param object an \code{mi} object
#' @param ... ignored parameters
#' @return a character-string specifying all the labels
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
labels.multi.dataset <- function (object, ...) {
  env <- object$state
  iter <- get('iter', envir=env)

  Reset(object)

  LABELS <- NULL

  while ({key <- NextLabel(object); !is.null(key)}) {
    Name <- key[[1]]
    Constraints <- key[-1]

    if (length(Constraints) == 0) {
      LABELS <- c(LABELS, Name)
      next
    }

    label <- paste(names(Constraints), Constraints, sep='=', collapse=", ")
    label <- sprintf("%s (%s)", Name, label)
    LABELS <- c(LABELS, label)
  }

  # Restore iterator to previous position
  assign('iter', iter, env)

  LABELS
}

# Multiset
multiple.frames <- function (obj, by = NULL) {
  UseMethod("multiple.frames")
}

# A set of things to make into a collection
multiple.frames.data.frame <- function (obj, labels = NULL, by = NULL) {
  labels <- as.character(substitute(obj))
  res <- list()
  res[[labels]] <- 
}

# An individual list
multiple.frames.list <- function (obj, by = NULL) {
}

# Takes a list of data frames *ONLY*
make.multi.dataset <- function (obj) {
  if (!inherits(obj, "list"))
    return(NULL)

  for (k in length(obj):1) {
    if (!is.data.frame(obj[[k]])) {
      warning('"mi" obj contains an element that is not a data.frame. Ommitting.')
      obj[[k]] <- NULL
    }
  }
}
