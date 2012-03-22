#' Method for Simulating Quantities of Interest for the ``relogit'' Model
#'
#' Simulaties quantities of interest
#' @usage \method{sim}{relogit}(obj,
#'                     x=NULL, x1=NULL, y=NULL,
#'                     num=1000, bootstrap = FALSE,
#'                     bootfn=NULL,
#'                     cond.data = NULL,
#'                     ...)
#' @S3method sim relogit
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
sim.relogit <- function(
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
  # If it is not a "Relogit2" class, then we just use a default sim call
  if (!inherits(obj$result, "Relogit2")) {

    # Get the call without expanding the dot-dot-dot parameters
    mc <- match.call(expand.dots = FALSE)

    # Storing a name
    mc[[1]] <- as.name("sim.default")

    # Return to prevent further evaluation of this method
    return(eval(mc))
  }

  # Warnings
  if (!is.null(cond.data))
    warning('The "cond.data" is not currently supported')

  # Get parameter estimate.
  # "as.parameters" is part of Zelig core
  message("<<<")

  res.param <- param.relogit2(obj, num=num, x=x)

  message("!")

  # Cast list into a "parameters" object
  res.param <- as.parameters(res.param)

  # Get quantities of interest
  res.qi <- qi(obj, x=x, x1=x1, y=y, param=param, num=num)
  
  # Cast list into a "qi" object. "as.qi" is part of Zelig core
  res.qi <- as.qi(res.qi)

  # Create s object.
  # I think this style of creating objects is kinda weird, but I want to try it
  # out.
  # - /\/\att Owen
  s <- structure(list(),
                 class = c("sim.relogit", "sim")
                 )

  # Individually define each slot with $ operator

  # Specify model name
  s$model <- obj$name

  # Specify setx objects used
  s$x <- x
  s$x1 <- x1

  # Summary information
  s$stats <- NULL

  #
  s$qi <- NULL
  s$titles <- NULL
  s$bootfn <- bootfn
  s$cond.data <- cond.data

  # Zelig object and its result
  s$zelig <- obj
  s$result <- obj$result

  # Calls made
  s$call <- match.call()
  s$zcall <- obj$call

  # Other imoprtant parameters
  s$num <- num
  s$special.parameters <- list(...)

  # Return
  s
}
