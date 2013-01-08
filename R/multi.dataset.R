# Make a ``multi.dataset'' Object
# @param datasets a list containing data.frames
# @param labels a character vector labeling indices of the dataset
make.multi.dataset <- function (datasets, labels=NULL) {
  md <- datasets

  if (!missing(labels))
    names(md) <- labels

  # Set super important attributes
  #attr(md, "something") <- "red"
  class(md) <- "multi.dataset"

  # Return object
  md
}

# Multiple Dataset Object
multi.dataset <- function (obj, ...) {
  UseMethod("multi.dataset")
}

# Create a Multiple Dataset Object from a data.frame
# @param obj a data.frame to conver
# @return a ``multi.dataset'' object
multi.dataset.data.frame <- function (obj, ...) {
  # Place inside a list
  make.multi.dataset(list(obj), "")
}

# Create a Multiple Dataset Object from a data.frame
# @param obj a list of data.frame's
# @return a ``multi.dataset'' object
multi.dataset.list <- function (obj, ...) {

  # Iterate backwards through list, so that we can remove elements
  for (k in length(obj):1) {
    if (!is.data.frame(obj[[k]])) {
      warning('"obj" contains an element that is not a data.frame... removing.')
      obj[[k]] <- NULL
    }
  }

  # Return object
  make.multi.dataset(obj, "")
}

# Create a Multiple Dataset Object from a data.frame
# @param obj a list of data.frame's
# @return a ``multi.dataset'' object
multi.dataset.amelia <- function (obj, ...) {
  data.frames <- obj$imputations
  class(data.frames) <- NULL
  make.multi.dataset(data.frames, names(data.frames))
}

# Divide a Data Frame or Matrix Into Subsets
# @param obj a data.frame or matrix to be split into subsets, divided by the
# categorical variable
# @param by a character-string, specifying the column to subset
# @return a list, containing the subsetted data sets. The names of the list
# correspond to the value of the subsetted list
divide <- function (obj, by) {

  # Ensure that "obj" is valid (a data.frame or matrix)
  if (!is.data.frame(obj) && !is.matrix(obj)) {
    warning('"obj" is not a data.frame or matrix')
    return(list(obj))
  }

  # Ensure that "by" is valid (a character-string)
  if (!is.character(by) && length(by) == 1) {
    warning('"by" is not a character-string')
    return(list(obj))
  }

  # Ensure that "by" is a column in "obj"
  if (! by %in% colnames(obj)) {
    warning('"by" is not a valid column of "obj"')
    return(list(obj))
  }

  # Get the set of possible values
  column.levels <-if (is.factor(obj[, by]))
    levels(obj[, by])
  else
    unique(obj[, by])


  # A list used to store each individual data.frame
  res <- list()

  # Iterate through all possible values and store each subset in a separate
  # entry in the list
  for (val in column.levels) {
    # Determine which rows match this value
    hits <- obj[, by] == val

    # Store data set temporarily in a local value
    data.set <- obj[hits, ]

    # Assign levels to the column. This adds levels to string data.
    levels(data.set[, by]) <- column.levels

    # Store data set in list
    res[[val]] <- data.set
  }

  # Return list
  res
}

# Print a ``multi.dataset'' Object
# @param x a multi.dataset object, essentially a list of data.frames
# @param ... parameters to pass to the print.data.frame object
# @return x (invisibly)
print.multi.dataset <- function (x, ...) {
  for (key in names(x)) {
    cat("label =", key, "\n")
    print(x[[key]], ...)
    cat("\n")
  }

  # Return printed object (invisibly)
  invisible(x)
}
