describe.negbin <- function(...) {
  # parameters object
  parameters <- list(pi = list(
                       equations = c(1, 1),
                       tags.allowed = FALSE,
                       dep.var = TRUE,
                       exp.var = TRUE
                       )
                     )

  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2008,
       category = "count",
       package  = package.zelig("CORE", 1.0),
       parameters = parameters,
       text = "Negative Binomial Regression for Event Count Dependent Variables"
       )
}
