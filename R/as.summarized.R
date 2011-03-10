as.summarized <- function(x, ...) {
  UseMethod("as.summarized")
}

as.summarized.summarized.qi <- function(x, ...) {
  x
}
