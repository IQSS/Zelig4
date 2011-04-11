# THIS FILE CONTAINS OPERATORS FOR USE WITH THE ZELIG PACKAGE
# -----------------------------------------------------------


# alias for [[ use
"[[.zelig" <- GetSlot.zelig


#
"[[.qi" <- function(self, key) {

  res <- attr(self, ".index")[[key]]

  if (is.null(attr(self, ".index")[[key]])) {
    NULL
  }
  else {
    do.call("$", list(self, attr(self, ".index")[[key]]))
  }
}


## "$.qi" <- function(self, key) {
##   key <- as.character(key)

##   self
## }

"%.%" <- function(f, g) {
  if (!(is.function(f) && is.function(g)))
    stop()
    
  function(...)
    f(g(...))
}


# @left: a list or vector
# @right: a list or vector
# return: left without any elements from right
# note: this is not commutative
#"%w/o%" <- function(left, right)
#  left[ ! left %in% right ]
