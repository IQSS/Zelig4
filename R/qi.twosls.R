#' Compute quantities of interest for 'twosls' Zelig models
#' @usage \method{qi}{twosls}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi twosls
#' @param obj a 'zelig' object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#' though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#' interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.twosls <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  # Compute the expected value of multistage LS methods
  compute.ev <- function (x, param, terms) {

    #
    if (is.null(x) || is.na(x)) {
      return(NA)
    }

    # Cast as data.frame
    x <- as.data.frame(x)

    # If 'x' has too many rows, there will currently be errors. This is an issue
    # in Zelig-core
    if (nrow(x) > 1) {
      warning("This package does not currently support pooled results.")
      x <- x[1, ]
    }

  }


  # Begin regular function
  terms <- terms(obj)

  # :q
  coef.list <- coef(param)

  # Hold Results
  eta <- list()

  #
  for (key in names(coef.list)) {
    #
    coef <- coef.list[[key]]
    # print(colnames(coef))
    small.x <- as.matrix(x$matrix[, colnames(coef)])
    #
    eta[[key]] <- coef %*% (small.x)
  }


  # Convert list into a matrix
  eta <- Reduce(function (x, y) cbind(x, y), eta)

  # Name each column after the associated equation
  colnames(eta) <- names(terms)

  # Return the results
  list(
       "Expected Value: E(Y|X)" = eta
       )
}
