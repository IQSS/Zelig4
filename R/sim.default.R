sim.default <- function(z,
                        x=NULL, x1=NULL, num=1000,
                        prev = NULL, bootstrap = FALSE,
                        boot.fn=NULL,
                        cond.data = NULL,
                        ...) {
  # error-catching
  if (!is.null(prev))
    warning("previous parameters are not yet supported")

  if (!is.null(boot.fn) || is.logical(bootstrap) && bootstrap)
    warning("bootstrapping is not yet implemented")

  if (!is.null(cond.data))
    warning("conditions are not yet supported")

  # "parameters"
  param <- as.parameters(param(z, num=num, bootstrap=bootstrap), num=num)

  # compute quantities of interest
  res.qi <- as.qi( qi(z, x=x, x1=x1, param=param, num=num) )

  # build object
  s <- list(name     = z$name,
            x        = x,
            x1       = x1,
            stats    = summarize(res.qi),
            qi       = res.qi,
            titles   = names(res.qi),
            boot.fn  = boot.fn,
            cond.data= cond.data,
            zelig.obj= z,
            call     = match.call(),
            zcall    = z$call,
            result   = z$result,
            iterations = num,
            special.parameters = list(...)
            )

  # cast class
  sim.class <- if (inherits(z, "MI"))
    sim.class <- "MI.sim"

  class(s) <- c(sim.class,
                z$name,
                paste("sim", z$name, sep="."),
                "sim"
                )

  # return
  s
}
