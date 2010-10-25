summary.sim <- function(s) {
  res <- list(model    = s$name,
              stats    = s$stats,
              titles   = s$titles,
              original = s$result,
              call     = s$call,
              zeligcall= s$zcall,
              x        = s$x,
              x1       = s$x1,
              iterations = s$iterations
              )
  class(res) <- c(s$name, "summary.sim")
  res
}
