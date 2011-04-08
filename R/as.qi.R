#' Generic method for casting various objects as
#' `qi' objects
#'
#' @param s the object to be casted
#' @return an object of type `qi'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.qi <- function(s)
  UseMethod("as.qi")


#' ??? -> qi
#'
#' @param s any unsupported object
#' @return an object of type `qi'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.qi.default <- function(s)
  stop("as.qi does not yet support this data-type")


#' qi -> qi
#'
#' @param s an object of type `qi'
#' @return s an object of type `qi'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.qi.qi <- function(s)
  s


#' list -> qi
#'
#' This function has a lot of room to go wrong. It tries
#' to detect whether the zelig model is old-style or
#' new-style (as of 4/4/2011). Eventually this should be
#' phased out.
#' 
#' @param s a list
#' @return an object of type `qi'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.qi.list <- function(s) {
  #q <- list(titles=list(), stats=list())
  titles <- list()
  stats <- list()

  # divide the list into ones with/without keys
  keys <- split.up(s)

  fail.names <- paste("qi", 1:length(s), sep="")
  success.names <- unlist(Map(.acronym, names(s), fail=''))
  success.names <- .number.list(success.names)

  # create new environment
  env <- new.env()

  # iterator
  k <- 1

  long  <- list()
  short <- list()
  stats <- list()

  # add the named entries
  for (title in names(keys$wordful)) {
    key <- if (regexpr("^[a-zA-Z]", success.names[k]) != -1)
      success.names[k]
    else
      ''

    stats[[key]] <- keys$wordful[[title]]
    long[[title]] <- key
    #attr(stats, title) <- key

    # increment counter
    k <- k + 1
  }

  attr(stats, ".index") <- long

  q <- stats

  #print(q)

  # cast as `qi' object, and return
  class(q) <- "qi"

  q    
}


#' Print a `qi' object in human-readable form
#'
#' @param x a qi object
#' @return the object that was printed
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.qi <- function(x, ...) {
  self <- x

  # error-catching
  if (length(self$titles) != length(self$stats))
    stop("corrupted object!  titles and stats length mismatch")

  qi.length <- length(self)

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

  invisible(x)
}


#
names.qi <- function(q) {
  nameless <- unlist(q$titles)
  names(nameless) <- NULL
  nameless
}


# pair-wise key assignment
iter.qi <- function(q) {
  #print(attr(q, ".index"))
  #print(q[["Expected Values: Pr(Y=j|X)"]])

  iter(
       Map(
           function(x) list(key=x, value=q[[x]]),
           names(attr(q, ".index"))
           )
       )
}
     


#
## length.qi <- function(q) {
##   len <- length(q$titles)

##   for (val in q$stats)
##     len <- len - all(is.na(val))

##   len
## }



.acronym <- function(str, fail=str) {
  ignored.words <- c(
                     "in", "for", "by",
                     "the", "a", "an"
                     )
  
  # remove all text after colon
  # remove trailing whitespace
  # remove leading whitespace
  # remove paranthetical statements
  reduced <- sub(':.*$', '', str)
  reduced <- sub('\\s+$', '', reduced, perl=TRUE)
  reduced <- sub('^\\s+', '', reduced, perl=TRUE)
  reduced <- gsub('\\(.*?\\)', '', reduced, perl=TRUE)

  
  # if we get an empty string, return whatever the fail value is
  if (nchar(reduced) < 1)
    return(fail)

  # splitted is not a word, I know
  #  1. split the reduced string into non-whitespace characters
  #  2. take the first letter of each
  #  3. put into lowercase
  splitted <- unlist(strsplit(reduced, '\\s+'))

  # remove ignored words
##   splitted <- Filter(
##                      function (char) regexpr(
##                      splitted
##                      )
  
  splitted <- substr(splitted, 1, 1)
  splitted <- tolower(splitted)

  # remove all non-letters
  acronym <- Filter(
                    function (char)
                    regexpr('^[a-zA-Z]$', char, perl=TRUE),
                    splitted
                    )

  # paste together, and return
  paste(acronym, sep="", collapse="")
}






# @vec: a vector of character-strings
.number.list <- function(vec) {

  #
  if (!is.character(vec)) {
    warning()
    return(vec)
  }

  #
  final.list <- c()
  unique.vec <- unique(vec)

  #
  for (k in 1:length(vec)) {

    val <- vec[k]

    hits <- sum(val == vec[1:k])
    total.hits <- sum(val == vec)

    final.list[names(vec)[k]] <- if (total.hits > 1)
      paste(val, hits, sep="")
    else
      val
  }


  # return
  final.list
}
