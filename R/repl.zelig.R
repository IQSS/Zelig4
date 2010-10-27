repl.sim <- function(object, x=NULL, x1=NULL, num=1000,
                     prev = NULL, bootstrap = FALSE,
                     boot.fn=NULL,
                     cond.data = NULL, ...) {
  # would rather use a factory function
  new.call <- object$call


  # this should always give the same value...
  rep.zelig <- eval(object$zcall, sys.parent())

  # 
  new.call$z <- rep.zelig

  # x
  new.call$x <- if (is.null(x))
    object$x
  else
    x

  # x1
  new.call$x1 <- if (is.null(x1))
    object$x1
  else
    x1

  # how is this EVER true?
  if (!is.null(object$seed))
    set.seed(object$seed)

  eval(new.call, sys.parent())
}
