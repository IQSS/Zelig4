#' Zelig2 bridge function
#'
#' ...
#' @param formula a formula object
#' @param model a chacter
#' @param ...
#' @param bias.correct ...
#' @param case.control ...
#' @param data a data.frame
#' @return ...
#' @export
zelig2relogit <- function(
                          formula,
                          ...,
                          tau = NULL,
                          bias.correct = NULL,
                          case.control = NULL,
                          data
                          ) {

  # Catch NULL case.control
  if (is.null(case.control))
    case.control <- "prior"

  # Catch NULL bias.correct
  if (is.null(bias.correct))
    bias.correct = TRUE

  # Construct formula. Relogit models have the structure:
  #   cbind(y, 1-y) ~ x1 + x2 + x3 + ... + xN
  # Where y is the response.
  form <- update(formula, cbind(., 1 - .) ~ .)

  # Set the environment to be this function's
  environment(form) <- environment()

  # Return the obvious answer
  list(
       .function = "relogit",
       formula = form,
       bias.correct = bias.correct,
       case.control = case.control,
       tau = tau
       )
}
