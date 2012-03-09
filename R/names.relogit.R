names.relogit <- function(x){
  res <- list(default=names(unclass(x)),
            estimate = names(x$lower.estimate), tau = x$tau)
  class(res) <- "names.relogit"
  res
}
