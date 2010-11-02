describe.poisson <- function(...) {
  # parameters object
  parameters <- list(lambda = list(
                       equations = c(1, 1),
                       tags.allowed = FALSE,
                       dep.vars = TRUE,
                       exp.vars = TRUE
                       )
                     )

  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "count",
       package  = package.zelig("CORE", 1.0),
       parameters = parameters,
       text = "Poisson Regression for Event Count Dependent Variables"
       )
}
