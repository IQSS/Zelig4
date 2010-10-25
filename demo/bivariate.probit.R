describe.bivariate.probit <- function() {
  category <- "bivaraite.dichotomous"
  package <- list(name = "mvtnorm", 
                  version = "0.7")
  mu <- list(equations = c(2,2),               # Systematic component 
             tagsAllowed = TRUE,          
             depVar = TRUE, 
             expVar = TRUE)
  rho <- list(equations = c(1,1),              # Optional systematic component
             tagsAllowed = FALSE,         #   Estimated as an ancillary
             depVar = FALSE,              #   parameter by default
             expVar = TRUE)
  pars <- list(mu = mu, rho = rho)
  list(category = category, parameters = pars)
}

zelig2bivariate.probit <- function(formula, model, data, M, ...) {
  Zelig:::packageConflicts("Matrix")
  require(mvtnorm)
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- as.name("bivariate.probit")
  as.call(mf)
}

bivariate.probit <- function(formula, data, start.val = NULL, ...) {

  # fml <- parse.formula(formula, req=c("mu1","mu2"), opt="rho") # [1]
  fml <- parse.formula(formula, model = "bivariate.probit")      # [1]
  D <- model.frame(fml, data = data)
  X <- model.matrix(fml, data = D, eqn = c("mu1", "mu2")) # [2a]
  # X <- model.matrix(fml, data = D, shape = "stacked",   # [2b]
  #                 eqn = c("mu1", "mu2"))
  # X <- model.matrix(fml, data = D, shape = "array",     # [2c]
  #                   eqn = c("mu1", "mu2"))
  Xrho <- model.matrix(fml, data = D, eqn = "rho")

  Y <- model.response(D)
  terms <- attr(D,"terms")
  start.val <- set.start(start.val, terms)
  start.val <- put.start(start.val, 1, terms, eqn = "rho")

  log.lik <- function(par, X, Y, terms) {
    Beta <- parse.par(par, terms, eqn = c("mu1", "mu2"))   # [3a]
    # Beta <- parse.par(par, terms, shape = "vector",      # [3b] & [3c]
    #                   eqn = c("mu1", "mu2"))             
    gamm <- parse.par(par, terms, eqn = "rho")
    rho <- (exp(Xrho %*% gamm) - 1) / (1 + exp(Xrho %*% gamm))

    mu <- X %*% Beta                                       # [4a]
    # mu <- X %*% Beta; mu <- matrix(mu, ncol = 2)         # [4b]
    # mu <- apply(X, 3, '%*%', Beta)                       # [4c]
    llik <- 0
    for (i in 1:length(rho)){
      Sigma <- matrix(c(1, rho[i], rho[i], 1), 2, 2)
      if (Y[i,1]==1)
        if (Y[i,2]==1)
          llik <- llik + log(pmvnorm(lower = c(0, 0), upper = c(Inf, Inf), 
                                     mean = mu[i,], corr = Sigma))
        else
          llik <- llik + log(pmvnorm(lower = c(0, -Inf), upper = c(Inf, 0), 
                                     mean = mu[i,], corr = Sigma))
      else
        if (Y[i,2]==1)
          llik <- llik + log(pmvnorm(lower = c(-Inf, 0), upper = c(0, Inf),
                                     mean = mu[i,], corr = Sigma))
        else
          llik <- llik + log(pmvnorm(lower = c(-Inf, -Inf), upper = c(0, 0), 
                                     mean = mu[i,], corr = Sigma))
        }
    return(llik)
  }

  res <- optim(start.val, log.lik, method = "BFGS",
               hessian = TRUE, control = list(fnscale = -1),
               X = X, Y = Y, terms = terms, ...)

  fit <- model.end(res, D) 
  class(fit) <- "bivariate.probit"
  fit
}

data(bivariate)

user.prompt()

## Shorthand notation for list(mu1 = y1 ~ x1, mu2 = y2 ~ x1)
z.out1 <- zelig(cbind(y1, y2) ~ x1, model = "bivariate.probit",
                data = bivariate)
z.out1$coef
user.prompt()

## Different explanatory variables for each response
z.out2 <- zelig(list(mu1 = y1 ~ x1, mu2 = y2 ~ x2),
                model = "bivariate.probit", data = bivariate)
z.out2$coef
user.prompt()
## Using tag() to constrain x1 to "gamma" in both formulae;
##  while mu1:x3 and mu2:x3 are estimated separately
z.out3 <- zelig(list(mu1 = y1 ~ tag(x1, "theta") + x3,
                     mu2 = y2 ~ tag(x1, "theta") + x3),
                model = "bivariate.probit", data = bivariate)
z.out3$coef
user.prompt()

## Using tag() to constrain mu1:x1 = mu2:x2 = "gamma",
##  and specifying an explanatory variable for rho.
z.out4 <- zelig(list(mu1 = y1 ~ tag(x1, "theta") + x2,
                     mu2 = y2 ~ tag(x2, "theta") + x3,
                     rho = ~ x4),
                model = "bivariate.probit", data = bivariate)
z.out4$coef
