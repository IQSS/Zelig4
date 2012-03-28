#' Default Boot-strapping procedure
#' 
#' The default procedure for extracting bootstrap information. Note that this
#' method re-fits the data and resamples the data frequently. This is a good
#' candidate for fixing-up.
#'
#' @param data a data.frame
#' @param i an integer or chacter-string specifying the index of the row to
#' be used in the bootstrapping procedure.
#' @param object the fitted model object
#' @param bootstrapfn a function used to bootstrap the object
#' @return a list of paramters
bootfn.default <- function(data, i, object, bootstrapfn=NULL, ...) {

  # Get a random sample of the data set
  d <- data[i,]

  # Extract the call object
  call <- .call

  # Replace the data frame with an appropriate one
  call$data <- d

  # Fit the model
  fit <- eval(call, sys.parent())

  # If "bootstrapfn" is unspecified, then we try to search its appropriate value
  # down
  if (is.null(bootstrapfn))
    bootstrapfn <- getS3method("bootstrap", .model, TRUE)

  # If bootstrap search came up sour, get default
  if (is.null(bootstrapfn))
    bootstrapfn <- Zelig:::bootstrap.default

  # Return bootstrap results
  res <- bootstrapfn(fit)

  # Return bootstrap result as vector
  res <- as.bootvector(res)

  # Return vectorized bootstrap simulation to "boot" function
  res$vector
}

#' Convert Boot Object to a Vector
#'
#' Receives a list with 2 slots as its input, and returns a vector of the two
#' smashed together alongwith the offsets used to reverse-construct the object.
#'
#' @note This method is used internally by Zelig to allow an intuitive,
#' ``param''-like API for bootstrapping.
#'
#' @param obj a list with two slots: ``alpha'' and ``beta''. Respectively, these
#' represent bootstrap samples for ancillary parameters and systematic
#' component of the bootstrapped GLM.
#' @return a list containing the resulting vector, as well as an object used to
#' reverse-build the list (``obj'') from the resulting call to ``bootstrap''.
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.bootvector <- function (obj) {

  # If this fails, something is really wrong.
  a <- obj$alpha
  b <- obj$beta

  # Error-checking
  if (!(is.vector(a) || is.null(a)))
    stop('The "alpha" slot of "obj" must be a vector.')

  if (!is.vector(b))
    stop('The "beta" slot of "obj" must be a vector.')

  # Return
  list(
       # For antiquity, beta should be placed before alpha. This is partially
       # because alpha is not always specified.
       vector = c(b, a),

       # The respective lengths of each vector
       lengths = c(beta = length(b), alpha = length(a)),

       # Names
       names = list(beta = names(b), alpha = names(a))
       )
}

#' Convert of Vector of Bootstrapped Parameters to a List-style Boot Object
#'
#' This inverts the ``as.bootvector'' function, and returns a list containing
#' the slots ``alpha'' and ``beta''.
#'
#' @param bootstraps ...
#' @param lengths ...
#' @return ...
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.bootlist <- function (bootstraps, lengths, names) {

  # Error-checking. "bootstraps" and "lengths" must:
  #  1. Both be vectors
  #  2. Must be the same length
  if (!is.matrix(bootstraps))
    stop('The parameter "bootstraps" must be a matrix')

  if (!is.vector(lengths))
    stop('The parameter "lengths" must be a vector.')

  if (sum(lengths) != ncol(bootstraps))
    stop('The parameters "bootstraps" and "lengths" must be ',
         'the same length.'
         )

  # Actual work begins here. This could be made more general, but if there's
  # more info besides "alpha" and "beta", it's not very much like a bootstrap...

  # Note that to make sense of the below, it has to be understood that the
  # canonical form of these bootstrapped values is:
  # (beta, alpha)
  # where beta is several columns of systematic parameters and
  # alpha is several columns of ancillary parameters
  a <- b <- NULL

  if (lengths[["beta"]] > 0) {
    # Subset the systematic parameter bootstraps according to their size
    b <- bootstraps[ , 1:lengths[["beta"]] ]

    # Change the column names of the system's parameter (beta) simulations
    colnames(b) <- names$beta
  }

  # Note that 1 + 1:2 is 2:3, so that this statement offsets subsetting by the
  # length of "a". 
  if (lengths[["alpha"]] > 0) {
    # Subset the ancillary parameter bootsraps to their size (and offset)
    a <- bootstraps[ , lengths[["beta"]] + 1:lengths[["alpha"]] ]

    # Change the column names of the ancillary parameter (alpha) simulations
    colnames(a) <- names$alpha
  }



  # Return the appropriate
  list(alpha = a, beta = b)
}
