# @...:   a set of lists to mix together
# return: all the combinations with repetition
#
# **note: maybe can be done better with recursion?
#         I think it might not got better than this
#         without having specific patterns known
.combine <- function(...) {
  # expand dot arguments
  dots <- list(...)

  # error-catching
  if (length(dots) < 1)
    return(NULL)

  # prepare lists for first iteration
  res <- dots[[1]]
  dots <- dots[-1]

  # this entire algorithm could be optimized,
  # however, it will always be exponential time
  while(length(dots) > 0) {
    # get list to store new combinations in
    new.list <- list()

    # divide list
    first <- dots[[1]]

    # add new combinations
    for (f in first)
      for (r in res)
        new.list[['']] <- c(r, f)

    # update list
    res <- new.list

    # shift first entry off
    dots <- dots[-1]
  }

  # m, as in matrix
  m <- NULL

  # format results as a matrix
  for (r in res)
    m <- rbind(m, r)

  # name rows/cols
  rownames(m) <- 1:length(res)
  colnames(m) <- names(list(...))

  # return
  m
}


# @dataf: a data.frame
# return: a data.frame with an overloaded operator
zframe <- function(dataf, labels=NULL) {
  # build list
  z <- list(data=dataf)

  # set class, and return
  class(z) <- c("zframe", "data.frame")
  z
}

"[.zframe" <- function(z, ...) {
  # expand dots
  dots <- list(...)

  # get keys
  keys <- Filter(nchar, names(dots))

  # but what if there's a list passed in?
  if (is.list(..1)) {
    dots <- ..1
    keys <- Filter(nchar, names(..1))
  }

  # get the data.frame
  zef <- z$data

  # if no keywords are specified, then 
  # we want to use the standard data.frame method
  if (length(keys) < 1)
    NextMethod(generic="[", object=zef)

  # filter each key-value pair
  for (key in keys) {
    # warn if a specified key doesn't exist
    # then skip
    if (! key %in% names(zef)) {
      warning()
      next
    }

    # get value to filter for
    val <- dots[[key]]

    # filter
     zef <- zef[zef[,key] == val,]
  }

  # return
  zef
}

#
as.data.frame.zframe <- function(zef)
  zef$data


#
chop.up <- function(...) split.up(list(...))

split.up <- function(args) {
  wordless <- list()
  wordful <- list()

  k <- 1

  if (is.null(names(args)))
    wordless <- c(...)

  for (key in names(args)) {
    if (nchar(key) == 0)
      wordless <- c(wordless, args[[k]])
    else
      wordful[[key]] <- args[[k]]

    k <- k+1
  }

  list(wordless=wordless, wordful=wordful)
}


