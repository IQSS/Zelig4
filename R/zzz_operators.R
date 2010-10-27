# THIS FILE CONTAINS OPERATORS FOR USE WITH THE ZELIG PACKAGE
# -----------------------------------------------------------


# alias for [[ use
"[[.zelig" <- GetSlot.zelig


#
"[[.qi" <- function(self, key)
  list(title=self$titles[[key]], stat=self$stats[[key]])


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
"%w/o%" <- function(left, right)
  left[ ! left %in% right ]
