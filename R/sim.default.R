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
#' of quantities of interest
#' @param y a parameter reserved for the computation of particular quantities of
#' interest (average treatment effects). Few models currently support this
#' parameter
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
  # Create environment of local variables
  model.env <- new.env()

  # Add local variables
  assign(".object", obj$result, model.env)
  assign(".fitted", obj$result, model.env)
  assign(".model", "model-name", model.env)

  # Get S3 methods
  paramfunction <- getS3method("param", obj$name, FALSE)
  qifunction <- getS3method("qi", obj$name, FALSE)
  bootfunction <- getS3method("bootstrap", obj$name, TRUE)

  parent.env(model.env) <- environment(paramfunction)

  environment(paramfunction) <- model.env
  environment(qifunction) <- model.env

  # Begin function

  if (length(attr(x, "pooled")) > 0 && attr(x, "pooled")) {

    xes <- list()
    titles <- NULL

    for (key in names(x)) {
      xes[[key]] <- sim(obj, x[[key]], x1[[key]], y, num, bootstrap, bootfn, cond.data, ...)
      attr(xes[[key]], "pooled") <- FALSE
      titles <- append(titles, xes[[key]]$titles)
    }

    attr(xes, "pooled") <- TRUE
    attr(xes, "pooled.setx") <- x
    attr(xes, "titles") <- unique(titles)

    class(xes) <- c("pooled.sim")

    return(xes)
  }

  # Stop on unimplemented features
  if (!is.null(cond.data))
    warning("conditions are not yet supported")

  # Simulate Parameters
  # param <- param(obj, num=num)
  param <- paramfunction(obj, num=num)

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

    # Add several private variables to bootfn:
    #   .fitted : a fitted model object
    #   .data : the data-set used to fit the original model
    #   .call : the call used to fit the original model
    #   .env : the environment in which the .call variable should/can be
    #          evaluated
    boot.env <- obj$method.env
    bootfn <- attach.env(bootfn, obj$method.env)

    # Bootstrapfn
    bootstrapfn <- getS3method("bootstrap", obj$name, TRUE)
    environment(bootstrapfn) <- model.env

    # If is.null then we just get the default bootstrap fn, which is merely to
    # simulate the systematic paramaters
    if (is.null(bootstrapfn))
      bootstrapfn <- Zelig:::bootstrap.default

    # Attach the appropriate environment to the function
    bootstrapfn <- attach.env(bootstrapfn, model.env)

    # Get a sample, so we know how to re-size the result.
    # Note: This "example" object will be used at the end of this if-clause to
    # build an object similar in structure to that of "bootstrapfn(obj)"
    example <- bootstrapfn(obj)
    example <- as.bootvector(example)

    # Bootstrap using a function with parameters: data, i, object
    # Where data is a data.frame, i is an vector of integers used to sample the
    # data.frame, and object is a fitted model object.
    res <- boot(d, bootfn, num,
                object = obj$result,
                bootstrapfn = bootstrapfn,
                num = num
                )

    # Copy the param object that was made earlier via ``param'' method
    res.param <- param

    # Reverse-construct a bootlist object from this
    bl <- as.bootlist(res$t, example$lengths, example$names)

    # Replace slots corresponding to "alpha" and "beta" on the "param" object
    param$coefficients <- bl$beta
    param$alpha <- bl$alpha
  }

  # Compute quantities of interest
  res.qi <- qifunction(obj, x=x, x1=x1, y=y, param=param, num=num)
  
  # Cast as a "qi" object if it is not one
  res.qi <- as.qi(res.qi)

  # Assign class
  class(res.qi) <- c(obj$name, class(res.qi))

  # This is kludge (for now)
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
            special.parameters = list(...),
            package.name = obj$package.name
            )

  # cast class
  sim.class <- if (inherits(obj, "MI"))
    sim.class <- "MI.sim"

  attr(s, "titles") <- unique(names(res.qi))

  class(s) <- c(sim.class,
                paste("sim", obj$name, sep="."),
                obj$name,
                "sim"
                )

  # return
  s
}

create.pooled.sim <- function(
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
  xes <- list()
  titles <- NULL

  for (key in names(x)) {
    xes[[key]] <- sim(obj, x[[key]], x1[[key]], y, num, bootstrap, bootfn, cond.data, ...)
    attr(xes[[key]], "pooled") <- FALSE
    titles <- append(titles, xes[[key]]$titles)
  }

  attr(xes, "pooled") <- TRUE
  attr(xes, "pooled.setx") <- x
  attr(xes, "titles") <- unique(titles)

  class(xes) <- c("pooled.sim")

  return(xes)
}
