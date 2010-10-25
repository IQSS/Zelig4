summary.MI.sim <- function(s) {
  res <- NextMethod()
  class(res) <- c(s$name, "summary.MI.sim", "summary.sim")
  res
}
