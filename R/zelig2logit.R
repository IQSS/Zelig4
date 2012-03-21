#' Interface between logit model and Zelig
#'
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param weights a numeric vector
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2logit <- function(formula, weights=NULL, robust = F, ..., data) {

  # Simply return
  list(
       .function = "glm",
       .hook = "robust.glm.hook",

       formula = formula,
       weights = weights,
       family  = binomial(link="logit"),
       model   = F,
       data    = data
       )
}
