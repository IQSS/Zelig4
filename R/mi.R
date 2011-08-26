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
make.mi <- function(obj, by=NULL) {

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

  # Return mi object
  self <- list(
               by     = by,
               frames = obj,
               state  = state,
               list   = datasets
               )
  class(self) <- "mi"
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
#' @return the next \code{data.frame} or NULL (if all \code{data.frames} have
#' generated
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
NextFrame <- function (obj) {
  key <- NextLabel(obj)

  if (is.null(key))
    return(NULL)

  Name <- key[1]
  Constraints <- key[-1]
  Data <- obj$frames[[Name]]

  for (col in names(Constraints)) {
    hits <- which(Data[, col] == Constraints[col])
    Data <- Data[hits, ]
  }

  Data
}

#' Reset Counter for \code{mi} Object
#'
#' ...

#' @note
#' @export
Reset <- function (obj) {
  assign('iter', 1, obj$state)
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
length.mi <- function(x) {
  nrow(x$list)
}
