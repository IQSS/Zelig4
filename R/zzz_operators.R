# THIS FILE CONTAINS OPERATORS FOR USE WITH THE ZELIG PACKAGE
# -----------------------------------------------------------


# alias for [[ use
"[[.zelig" <- GetSlot.zelig


#
"[[.qi" <- function(self, key)
  list(title=self$titles[[key]], stat=self$stats[[key]])


"%.%" <- function(f, g) {
  if (!is.function(c(f, g)))
    stop()
    
  function(...)
    f(g(...))
}
