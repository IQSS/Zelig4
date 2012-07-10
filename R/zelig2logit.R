#' Interface between logit model and Zelig
#'
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param weights a numeric vector
#' @param robust a boolean (logical) specifying whether robust error estimates
#' should be used
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2logit <- function(formula, weights=NULL, robust = F, ..., data) {
  w <- weights
  z(
    glm,
    # .hook = "robust.glm.hook",
    formula = formula,
    weights = w,
    family  = binomial(link="logit"),
    model   = F,
    data    = data
    )
}
