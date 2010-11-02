describe.probit <- function(...){
  parameters <-list(mu = list(
                      equations = c(1,1),
                      tags.allowed = FALSE,
                      dep.vars = TRUE,
                      exp.vars = TRUE
                      )
                    )
  
  # return
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "dichotomous",
       package  = package.zelig("CORE", 1.),
       parameters = parameters,
       text = "Probit Regression for Dichotomous Dependent Variables"
       )
}
