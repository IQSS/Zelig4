#' Generate Formulae that Consider Clustering
#'
#' This method is used internally by the "Zelig" Package to interpret
#' clustering.
#' @param formula a formula object
#' @param cluster a vector
#' @return a formula object describing clustering
cluster.formula <- function (formula, cluster) { 

  # Convert LHS of formula to a string
  lhs <- deparse(formula[[2]])

  cluster.part <- if (is.null(cluster))
    # NULL values require
    sprintf("cluster(1:nrow(%s))", lhs)

  else
    # Otherwise we trust user input
    sprintf("cluster(%s)", cluster)

  update(formula, paste(". ~ .", cluster.part, sep=" + "))
}
