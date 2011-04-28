# This file contains overloaded operators 
# However, developers - in general - should avoid the use of these features,
# and instead use iterators when dealing with multiple fitted models or
# quantities of interest.
# The methods primarily come up when defining 'summarize' and 'plot' functions


#' Extract a Value from a Fitted Model Object (Wrapped by Zelig)
#' @S3method "[[" zelig
#' @param z an object of type 'zelig'
#' @param slot a character-string specifying the slot to extract from the fitted
#'   model object
#' @param ... subsequent slots to extract from the fitted model object
#' @return contents of the specified slots
#' @author Matt Owen \emph{mowen@@iq.harvard.edu}
"[[.zelig" <- GetSlot.zelig

#' Extraction Operator for Quantities of Interest
#' This function is exclusively used internally by Zelig, and behaves in a very
#' fishy manner. \code{qi} objects maintain an internal list of indices which
#' are used to find the appropriate slot which holds a particular quantity of
#' interest.
#' When a \code{qi} object is defined, all the quantities of interest are
#' converted into acronyms, so that elements of the \code{qi} object can be
#' stored without a lengthy name containing spaces (since most qi's are
#' human-readable). As a result, these objects contain an \code{.index}
#' attribute which pairs every quantity of interest with its acronym. This
#' index is then used to extract (using the \code{$} operator) the appropriate
#' element of the list.
#' In short, it pairs the key "Expected Value" with the slot \code{ev}. This
#' allows that the following will always be true (in the mentioned example):
#'   \code{qi$ev == qi[["Expected Value"]]}
#' @note When possible, \code{qi} objects should be handled with iterators
#'   rather than list-style extraction operators.
#' @S3method "[[" qi
#' @param self the \code{qi} object
#' @param key a character-string specifying the title of the quantity of
#'   interest to extract.
#' @return if the quantity of interest exists, that entry. Otherwise,
#'   \code{NULL}
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
"[[.qi" <- function(self, key) {

  # produce the index of titles of qi's
  index <- attr(self, ".index")

  # find the 'short-name' matching
  qi.short.name <- index[[key]]

  if (is.null(qi.short.name))
    NULL
  else
    # if this title => key pair is found, invoke the "$" operator on the
    # shortname. In effect, this makes:
    #   qi[['Expected Value']]
    #
    # equivalent to:
    #   qi$ev
    do.call("$", list(self, qi.short.name))
}


## "$.qi" <- function(self, key) {
##   key <- as.character(key)

##   self
## }

# @left: a list or vector
# @right: a list or vector
# return: left without any elements from right
# note: this is not commutative
#"%w/o%" <- function(left, right)
#  left[ ! left %in% right ]
