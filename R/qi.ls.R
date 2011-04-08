#' Compute quantities of interest for `ls' Zelig models
#'
#' @param obj a `zelig' object
#' @param x a `setx' object or NULL
#' @param x1 an optional `setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#'          though this feature is currentlysupported by only a
#'          handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of
#'         quantities of interest with their simulations
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.ls <- function(z, x, x1=NULL, y=NULL, num=1000, param=NULL, bootstrap=NULL) {
  # error-catching
  if (missing(x))
    stop("x cannot be missing while computing the `ls' model")

  # get `parameters'... whatever that means
  coefs <- coef(param)

  # compute expected value
  ev <- coefs %*% t(x)
  ev1 <- NA
  fd <- NA
  
  if (!is.null(x1)) {
    ev1 <- coefs %*% t(x1)
    fd <- ev1 - ev
  }

  # return
  list("Expected Value: E(Y|X)" = ev,
       "Expected Value (of X1): E(Y|X1)" = ev1,
       "First Difference in Expected Values: E(Y|X1) - E(Y|X)" = fd
       )
}
