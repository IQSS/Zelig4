describe.ls <- function(...){
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
       category = "continuous",
       package  = package.zelig("stats", .1),
       parameters = parameters,
       text = "Least Squares Regression for Continuous Dependent Variables"
       )
}
