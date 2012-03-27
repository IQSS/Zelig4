#' Method for Simulating Quantities of Interest wrom 'zelig' Objects
#'
#' Simulate quantities of interest
#' @usage \method{sim}{default}(obj,
#'                     x=NULL, x1=NULL, y=NULL,
#'                     num=1000, bootstrap = FALSE,
#'                     bootfn=NULL,
#'                     cond.data = NULL,
#'                     ...)
#' @S3method sim default
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
#' @param ... parameters to be passed to the boot function, if one is supplied
#' @return a 'sim' object storing the replicated quantities of interest
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
sim.default <- function(
                        obj,
                        x = NULL,
                        x1 = NULL,
                        y = NULL,
                        num = 1000,
                        bootstrap = FALSE,
                        bootfn = NULL,
                        cond.data = NULL,
                        ...
                        ) {
  # Stop on unimplemented features
  if (!is.null(cond.data))
    warning("conditions are not yet supported")

  # Simulate Parameters
  param <- param(obj, num=num)

  # Cast list into a "parameters" object
  param <- as.parameters(param, num)

  # Define the pre-sim hook name
  post.hook <- obj$zc$.post

  # apply the hook if it exists
  if (!is.null(post.hook)) {
    zelig2 <- get(paste("zelig2", obj$name, sep=""))
    envir <- environment(zelig2)

    # Produce a warning if the post-hook defined cannot be found
    if (!exists(post.hook, mode="function", envir=envir))
      warning("the hook '", post.hook, "' cannot be found")
    
    # Otherwise, business as usual. Extract the hook and apply it to the zelig
    # object. Note that a post-hook always has the arguments:
    #   obj, x, x1, bootstrap, bootfn, param
    else {
      # Retrieve the hook, since it exists
      hook <- get(post.hook, envir=envir)

      # Assign the param object. In the case of bootstrapping, the param object
      # might not have any meaning.
      param <- if (bootstrap)
        param

      # Otherwise apply the hook and return it as the parameters
      else
        hook(obj, x, x1, bootstrap, bootfn, param=param)
    }
  }

  # Get default boot-strapping function if boot is enabled and no boot-function
  # is specified
  if (bootstrap && missing(bootfn))
    bootfn <- bootfn.default

  # Boot-strapping!!
  if (!missing(bootfn) && !is.null(bootfn)) {

    # Get the appropriate 
    d <- obj$data
    d <- d[complete.cases(d), ]

    # Bootstrap?
    res <- boot(obj$data, bootfn, R = num, object = obj)

    # Copy the param object that was made earlier via ``param'' method
    res.param <- param

    # Overwrite the parameters were produced iby the ``param'' method with the
    # bootstrapped values (so sick)
    param$coefficients <- res$t


    print(names(obj))
    q()

    # Name the parameters appropriately
    colnames(param$coefficients) <- names(res$t0)
  }


  # Compute quantities of interest
  res.qi <- qi(obj, x=x, x1=x1, y=y, param=param, num=num)
  
  # Cast as a "qi" object if it is not one
  res.qi <- as.qi(res.qi)

  # Assign class
  class(res.qi) <- c(obj$name, class(res.qi))

  # this is kludge (for now)
  # This can be removed as of 4-27-2011
  if (inherits(obj, "MI"))
    class(res.qi) <- c("MI", class(res.qi))



  # build object
  s <- list(
            model     = obj$name,
            x        = x,
            x1       = x1,
            stats    = summarize(res.qi),
            qi       = res.qi,
            titles   = names(res.qi),
            bootfn   = bootfn,
            cond.data= cond.data,
            zelig    = obj,
            call     = match.call(),
            zcall    = obj$call,
            result   = obj$result,
            num      = num,
            special.parameters = list(...)
            )

  # cast class
  sim.class <- if (inherits(obj, "MI"))
    sim.class <- "MI.sim"

  class(s) <- c(sim.class,
                paste("sim", obj$name, sep="."),
                obj$name,
                "sim"
                )

  # return
  s
}
