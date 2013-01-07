#
#
#
mi <- function () {
}

#
#
#
multi.dataset <- function (obj) {
}

#
#
#
multiset <- function (obj, ...) {
}

#
#
#
multiset.list <- function (obj, ...) {
}

#
#
#
multiset.amelia <- function (obj, ...) {
}

# Divide a Data Frame or Matrix
#
#
dissect <- function (obj, by) {

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

  #
  values <- obj[, by]

  column.levels <-if (is.factor(values))
    levels(values)
  else
    unique(values)


  # A list used to store each individual data.frame
  res <- list()

  # 
  for (val in column.levels) {
    # Matches in the data.frame
    hits <- obj[, by] == val
    data.set <- obj[hits, ]
    levels(data.set[, by]) <- column.levels

    res[[val]] <- data.set
  }

  res
}

LEVELS <- c("red", "dead", "revolver")
col <- sample(LEVELS, 30, TRUE, c(.5, .5, .0))
col <- factor(col, LEVELS)

d <- data.frame(x = col, t = 1:length(col))

dissect(d, "x")[[1]][1, 1]

#
