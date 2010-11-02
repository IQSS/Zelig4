describe.gamma <- function(...) {
  # parameters object
  parameters <- list(lambda = list(
                       equations = c(1, 1),
                       tags.allowed = FALSE,
                       dep.var = TRUE,
                       exp.var = TRUE
                       )
                     )

  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "bounded",
       package  = package.zelig("CORE", 1.0),
       parameters = parameters,
       text = "Gamma Regression for Continuous, Positive Dependent Variables"
       )
}
