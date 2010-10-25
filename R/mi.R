# generic function
mi <- function(..., by=NULL) {
  UseMethod("mi")
}


# @...:   a set of data.frames
# return: mi object
mi.default <- function(..., by = NULL) {
  data.labels <- match.call(expand.dots=F)[["..."]]
  data.labels <- as.character(data.labels)

  mi(list(...), by = by)
}


# @m:     mi function
# return: whatever was passed in
mi.mi <- function(m, ..., by=NULL) {
  if (missing(by))
    by <- m$by

  mi(m$data, by=by, data.labels=m$labels)
}

# @lis:   a list of data.frames
# return: mi object
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


# @...:   generic arguments
# return: ideally, nothing
reset <- function(...) UseMethod("reset")


# @m:     mi object
# return: next data.frame in iterator,
#         or stopIteration error
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


# @m:     mi object
# return: nothing.
reset.mi <- function(m) {
  assign('i', 0, env=m$iter$state)
  invisible(m)
}


# @m: 'mi' object
# return: number of data-frame subsets
length.mi <- function(m)
  nrow(m$iter$state$obj)
