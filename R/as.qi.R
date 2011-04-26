#' Generic Method for Converting Various Objects into 'qi' Objects
#' 'qi' objects are list-style objects used by the 'summarize' function to 
#' compute simple summaries about the simulated data. For readability and
#' and simplicity purposes, the 'qi' function typically returns a list of
#' named simulations. This list is converted internally by Zelig into a 'qi'
#' object so that several methods can be easily applied to the Quantities of
#' Interest: plot, summarize, and print
#' @note These functions are primarily used internall by Zelig and should not
#'   be used in the Global namespace.
#' @param s the object to be casted
#' @return an object of type `qi'
#' @seealso as.qi.default as.qi.qi as.qi.list
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
#' This function has a lot of room to go wrong. It tries o detect whether the
#' zelig model is old-style or new-style (as of 4/4/2011). Eventually this
#' feature should be phased out.
#' @note This method has peculiar behavior when the list contains only two
#' elements. The crucial fix is to simply remove the portion of code which
#' intentionally implements this perculiar behavior.
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


#' The Names of a 'qi' Object
#' Function to get the names of a 'qi' object. This function does not entirely
#' parallel the functionality of traditional 'names' methods; this is because
#' the \code{$} operator has been overloaded to support a unique style of value
#' extraction. For technical details, please see the source code.
#' @note No method exists to set the names of a 'qi' object, once it is 
#'   constructed. This will be a feature added later.
#' @param q a 'qi' object
#' @return a character-vector containing the names of the Quantities of
#'   Interest
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
names.qi <- function(x) {
  nameless <- unlist(x$titles)
  names(nameless) <- NULL
  nameless
}


#' Construct an Iterator from a 'qi' Object
#' @param obj a 'qi' object
#' @param ... ignored parameters
#' @return an Iterator of lists containing two keys: key and value. This is 
#'   so that every simulated Quantity of Interest comes paired with its title.
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
iter.qi <- function(obj, ...) {
  q <- obj
  iter(
       Map(
           function(x) list(key=x, value=q[[x]]),
           names(attr(q, ".index"))
           )
       )
}


#' Convert a Vector of Character Strings into Acronyms
#' This function will convert a vector of character strings into their
#' appropriately titled acronym forms. That is, the two Quantity of Interest
#' titles:
#' \begin{itemize}
#'    \item "Expected Values (for X): E(Y|X)"
#'    \item "Expected Values (for X1): E(Y|X1)"
#' \end{itemize}
#' The result will be: "ev" and "ev". That is, the acronym will not contain
#' information kept in paranetheses or after a colon. 
#' @note This function currently includes preopositions as parts of acroynms
#' @param str a vector of character strings to convert into acronymns
#' @param fail a result to produce upon failure
#' @return a vector of character-strings
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
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


#' Append Numbers to Identically Valued Strings
#' This function ensures that vectors of strings are uniquely named.
#' @note This function is used in tandem with '.acronym' to correctly produce
#'   short-names for quantities of interest.
#' @param vec a vector of character-string
#' @return a vector of character-strings of shorter length. Duplicate hits on
#'   short-titled names append a number to the end. E.g.: the character vector
#'   if vec equals c('ev', 'ev', 'pr'), then the result will be:
#'   c('ev1', 'ev2', 'pr')
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.number.list <- function(vec) {
  if (!is.character(vec)) {
    warning()
    return(vec)
  }

  final.list <- c()
  unique.vec <- unique(vec)

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
