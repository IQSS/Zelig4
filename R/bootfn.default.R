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
#' @param num an integer specifying the number of samples to simulate
#' @param ... unspecified parameters
#' @return a list of paramters
bootfn.default <- function(data, i, object, bootstrapfn=NULL, num, ...) {

  # This is mostly here to squelch R-check notes, however sloppy programming
  # can potentially prevent the ".model" variable from being defined in the
  # attached environment. To make sense of this line, see the "sim.default"
  # function where an environment (containing the variable ".model"  is
  # explicity attached to the boot function
  if (!exists(".model"))
    .model <- "default"

  # Get a random sample of the data set
  d <- data[i,]

  # Extract the call object
  # Naming it "jeez" because it's really hard to find names for call objects
  # that are meaningful and not reserved for other functions
  jeez <- .call

  # Replace the data frame with an appropriate one
  jeez$data <- d

  .env <- if (exists('.env'))
    .env
  else
    NULL

  # Fit the model
  fit <- eval(jeez)

  # If "bootstrapfn" is unspecified, then we try to search its appropriate value
  # down
  if (is.null(bootstrapfn))
    bootstrapfn <- getS3method("bootstrap", .model, TRUE)

  # If bootstrap search came up sour, get default
  
## CRAN is opposed to ::: within same package, 
## but I'm opposed to S4 environment artifacts
##  if (is.null(bootstrapfn))
##    bootstrapfn <- Zelig:::bootstrap.default
## So this obviously makes my code better:

  if (is.null(bootstrapfn)){
    localbootstrap.default <- function (obj, ...)
    list(
       alpha = NULL,
       beta = coef(obj)
       )
    bootstrapfn <- localbootstrap.default
  }


  # Attach the ".num" private variable
  bootstrapfn <- attach.env(bootstrapfn, NULL, .num = num, .fitted = object)

  # Get a result
  res <- bootstrapfn(fit)

  # Return vectorized bootstrap simulation to "boot" function
  as.bootvector(res)$vector
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
    stop('The "alpha" slot of "obj" must be a vector or NULL.')

  if (!(is.vector(b)))
    stop('The "beta" slot of "obj" must be a vector')

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
#' @param names a character-vector specifying the names of the boot terms
#' @return ...
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.bootlist <- function (bootstraps, lengths, names) {

  # Error-checking. "bootstraps" and "lengths" must:
  #  1. "bootstraps" must be a matrix && have at least 1 value
  #  2. "lengths" must be a vector
  #  3. The sum of all the lengths must perfectly add up to the number of
  #     columns in bootstraps
  if (!is.matrix(bootstraps) && ncol(bootstraps) > 0 && nrow(bootstraps) > 0)
    stop('The parameter "bootstraps" must be a matrix')

  if (!is.vector(lengths))
    stop('The parameter "lengths" must be a vector.')

  if (sum(lengths) != ncol(bootstraps))
    stop('The parameters "bootstraps" and "lengths" must be ',
         'the same length.'
         )

  # Actual work begins here. This could be made more general, but if there's
  # more info besides "alpha" and "beta", it's not very much like a bootstrap...
  # In the future, we might need to add support for "link", "inverse link" and
  # "family" slots, but there is overlap here with the "param" method.

  # Note that to make sense of the below, it has to be understood that the
  # canonical form of these bootstrapped values is:
  # (beta, alpha)
  # where "beta" is several columns of systematic parameters and
  # "alpha" is several columns of ancillary parameters
  a <- b <- NULL

  # If beta is 0-sized, then we should ignore it
  if (lengths[["beta"]] > 0) {
    # Extract the "beta" portion of "bootstraps". These values should represent
    # the systematic parameters
    b <- bootstraps[ , 1:lengths[["beta"]] ]

    # Change the column names of the system's parameter (beta) simulations
    b <- name.object(b, names$beta)
  }

  # Note that 1 + 1:2 is 2:3, so that this statement offsets subsetting by the
  # length of "a". 
  if (lengths[["alpha"]] > 0) {
    # Extract several columns from "bootstraps". These values should represent
    # the model's ancillary parameters
    a <- bootstraps[ , lengths[["beta"]] + 1:lengths[["alpha"]] ]

    # Change the column names of the ancillary parameter (alpha) simulations
    a <- name.object(a, names$alpha)
  }

  # Return the appropriate
  list(alpha = a, beta = b)
}

#' Name Elements of an Object
#'
#' Returns an object
#' @note This method is used internally by Zelig to name the columns and
#' elements of matrices and vectors for simulations and bootstrapped parameters.
#' @param obj a vector or matrix
#' @param names a character-vector specifying names
#' @return the original object, with a "colnames" or "names" equal to the
#' parameter "names". If "names" is larger than "obj", the "names" parameter
#' is truncated appropriately. If it is smaller, then the latter part of "obj"
#' is replaced with a numbered generic column name
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
name.object <- function (obj, names) {

  # Handle the special case, which shouldn't really happen...
  if (is.null(names)) {
    if (is.matrix(obj))
      colnames(obj) <- NULL
    else if (is.vector(obj))
      names(obj) <- NULL
    return(obj)
  }

  # Get the length of names
  names.len <- length(names)

  # Get the 'length' of the object, regardless of whether it is a vector or
  # matrix. Note that in our case, length is equivalient to "ncol" if the
  # object is a matrix
  obj.len <- if (is.matrix(obj))
    ncol(obj)
  else if (is.vector(obj))
    length(obj)
  else {
    # Warn the user. This might be necessary, but it helps debug for
    # developers. Ideally this case never crops up in well-made Zelig models
    warning('"name.object" ignores objects that are not matrices or vectors')

    # Bail out of the function
    return(obj)
  }

  # Ensure that names is the exact length of "obj" by
  if (names.len < obj.len) {
    # Create vector equal in size to the length of the object being named
    temp <- paste(rep("col", obj.len), 1:obj.len, sep = "")

    # Replace default values (col1, col2, ... colN) with the value that
    # *should* there in a perfect world, where there is never any glitchy code
    temp[1:names.len] <- names

    # Replace "names" with the newly constructed, appropriately size, vector
    # of names
    names <- temp
  }

  # Truncate the "names" parameter if it is too largit is too large
  else if (names.len > obj.len) {
    # Warn the user. This is probably only useful/meaningful to developers. 
    # This case should not crop up in well made Zelig models.
    warning('"names.object" is truncating the names parameter, because it ',
            'is larger than "obj" the object of the function.')

    # Truncate "names"
    names <- names[1:obj.len]
  }

  # After all the prep work, finally name the object
  if (is.matrix(obj))
    colnames(obj) <- names

  else if (is.vector(obj))
    names(obj) <- names

  else
    warning('"obj" must be a matrix or a vector. ',
            'Returning the "obj" untouched.')

  # Return modified object
  obj
}
