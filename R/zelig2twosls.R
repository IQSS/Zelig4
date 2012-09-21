#' Interface between the Zelig Model twosls and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2twosls <- function (formula, ..., data) {

  # Helper function to perform set-difference
  "%w/o%" <- function(x, y)
    x[!x %in% y]

  formula<-parse.formula(formula, "twosls")
  tt<-terms(formula)

  ins<-names(tt) %w/o% names(attr(tt,"depVars"))
  if(length(ins)!=0)
    if(length(ins)==1)
      inst <- formula[[ins]]
    else 
      inst <- formula[ins]

  else
    stop("twosls model requires instrument!!\n")

  class(formula) <- c("multiple", "list")

  # Return
  list(
       .function = "callsystemfit",
       formula = formula[names(attr(tt,"depVars"))],
       method  = "2SLS",
       inst    = inst,
       data = data,
       ...
       )
}
