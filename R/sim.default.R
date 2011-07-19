#' Method for Simulating Quantities of Interest wrom 'zelig' Objects
#' @param obj a 'zelig' object
#' @param x a 'setx' object
#' @param x1 a secondary 'setx' object used to perform particular computations
#'   of quantities of interest
#' @param y a parameter reserved for the computation of particular quantities of
#'   interest (average treatment effects). Few models currently support this
#'   parameter
#' @param num an integer specifying the number of simulations to compute
#' @param bootstrap ignored
#' @param bootfn ignored
#' @param cond.data ignored
#' @param ... special parameters which are reserved for future versions of Zelig
#' @return a 'sim' object storing the replicated quantities of interest
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
sim.default <- function(obj,
                        x=NULL, x1=NULL, y=NULL,
                        num=1000, bootstrap = FALSE,
                        bootfn=NULL,
                        cond.data = NULL,
                        ...) {
  # error-catching
  if (!is.null(bootfn) || is.logical(bootstrap) && bootstrap)
    warning("bootstrapping is not yet implemented")

  if (!is.null(cond.data))
    warning("conditions are not yet supported")

  # parameters
  param <- as.parameters(param(obj, num=num), num=num)

  # define the pre-sim hook name
  post.hook <- obj$zc$.post

  # apply the hook if it exists
  if (!is.null(post.hook)) {
    zelig2 <- get(paste("zelig2", obj$name, sep=""))
    envir <- environment(zelig2)

    if (!exists(post.hook, mode="function", envir=envir))
      warning("the hook '", post.hook, "' cannot be found")
    
    else {
      hook <- get(post.hook, envir=envir)

      param <- if (bootstrap) {
        warning("bootstrap does not currently support hook functions")
        param
      }
      else {
        hook(obj, x, x1, bootstrap, bootfn, param=param)
      }
    }

  }
  

  # compute quantities of interest
  res.qi <- as.qi( qi(obj, x=x, x1=x1, y=y, param=param, num=num) )
  class(res.qi) <- c(obj$name, class(res.qi))

  # this is kludge (for now)
  # This can be removed as of 4-27-2011
  if (inherits(obj, "MI"))
    class(res.qi) <- c("MI", class(res.qi))


  # build object
  s <- list(name     = obj$name,
            x        = x,
            x1       = x1,
            stats    = summarize(res.qi),
            qi       = res.qi,
            titles   = names(res.qi),
            bootfn   = bootfn,
            cond.data= cond.data,
            zelig.obj= obj,
            call     = match.call(),
            zcall    = obj$call,
            result   = obj$result,
            iterations = num,
            special.parameters = list(...)
            )

  # cast class
  sim.class <- if (inherits(obj, "MI"))
    sim.class <- "MI.sim"

  class(s) <- c(sim.class,
                obj$name,
                paste("sim", obj$name, sep="."),
                "sim"
                )

  # return
  s
}
