# generic
as.qi <- function(s) UseMethod("as.qi")


# default
as.qi.default <- function(self)
  stop("as.qi does not yet support this data-type")


# do nothing, obviously
as.qi.qi <- function(q) q


# list
as.qi.list <- function(s) {
  q <- list(titles=list(), stats=list())

  # divide the list into ones with/without keys
  keys <- split.up(s)

  # add untitled entries, and give them a title
  if (length(keys$wordless)) {
    for (entry.count in 1:length(keys$wordless)) {
      # title the untitled qi's
      title <- gettext("Untitled QI #")
      title <- paste(title, entry.count, sep="")

      # add entries
      q$titles[['']] <- title
      q$stats[['']] <- keys$wordless[[entry.count]]
    }
  }

  # add the named entries
  for (title in names(keys$wordful)) {
    # add entries
    q$titles[['']] <- title
    q$stats[['']] <- keys$wordful[[title]]
  }

  # cast as `qi' object, and return
  class(q) <- "qi"
  q    
}


# @qi:    qi object
# result: nothing
# effect: prints qi object
print.qi <- function(self) {
  # error-catching
  if (length(self$titles) != length(self$stats))
    stop("corrupted object!  titles and stats length mismatch")

  qi.length <- length(self$titles)

  # iterate through
  for (k in 1:qi.length) {
    # output title
    message(self$titles[[k]])

    # output qi
    print(self$stats[[k]])

    # just to prevent extra end-line
    if (k != qi.length)
      message()
  }
}


#
names.qi <- function(q) unlist(q$titles)


# pair-wise key assignment
iter.qi <- function(q)
  iter(Map(function(x, y) list(key=x, value=y), q$titles, q$stats))


#
length.qi <- function(q) {
  len <- length(q$titles)

  for (val in q$stats)
    len <- len - all(is.na(val))

  len
}
