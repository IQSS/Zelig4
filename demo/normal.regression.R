describe.normal.regression <- function() {
  category <- "continuous"
  mu <- list(equations = 1,              # Systematic component
             tagsAllowed = FALSE, 
             depVar = TRUE, 
             expVar = TRUE)
  sigma2 <- list(equations = 1,          # Scalar ancillary parameter
                 tagsAllowed = FALSE, 
                 depVar = FALSE, 
                 expVar = FALSE)
  pars <- list(mu = mu, sigma2 = sigma2)
  model <- list(category = category, parameters = pars)
}

zelig2normal.regression <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)                     # [1]
  mf$model <- mf$M <- NULL                                 # [2]
  mf[[1]] <- as.name("normal.regression")                  # [3]
  as.call(mf)                                              # [4] 
}

normal.regression <- function(formula, data, start.val = NULL, ...) {
  # fml <- parse.formula(formula, req = "mu", ancil = "sigma2")  # [1a]
  fml <- parse.formula(formula, model = "normal.regression") # [1b]
  D <- model.frame(fml, data = data)
  X <- model.matrix(fml, data = D)
  Y <- model.response(D)
  terms <- attr(D, "terms")
                                    
  start.val <- set.start(start.val, terms)                     # [2]

  ll.normal <- function(par, X, Y, n, terms) {                 # [3]
    beta <- parse.par(par, terms, eqn = "mu")                  # [3a]
    gamma <- parse.par(par, terms, eqn = "sigma2")             # [3b]
    sigma2 <- exp(gamma)
    -0.5 * (n * log(sigma2) + sum((Y - X %*% beta)^2 / sigma2)) 
  }

  res <- optim(start.val, ll.normal, method = "BFGS",          # [4]
               hessian = TRUE, control = list(fnscale = -1),
               X = X, Y = Y, n = nrow(X), terms = terms, ...)      

  fit <- model.end(res, D)                                     # [5]
  class(fit) <- "normal"                                    
  fit                                                        
}

param.normal <- function(object, num = NULL, bootstrap = FALSE, 
                   terms = NULL) {
  if (!bootstrap) {
    par <- mvrnorm(num, mu = coef(object), Sigma = vcov(object))
    Beta <- parse.par(par, terms = terms, eqn = "mu")
    sigma2 <- exp(parse.par(par, terms = terms, eqn = "sigma2"))
    res <- cbind(Beta, sigma2)
  }
  else {
    par <- coef(object)
    Beta <- parse.par(par, terms = terms,  eqn = "mu")
    sigma2 <- exp(parse.par(par, terms = terms, eqn = "sigma2"))
    res <- c(coef, sigma2)
  }
  res
}

qi.normal <- function(object, par, x, x1 = NULL, y = NULL) {
  Beta <- parse.par(par, eqn = "mu")
  sigma2 <- parse.par(par, eqn = "sigma2")
  ev <- Beta %*% t(x)    
  pr <- matrix(NA, ncol = ncol(ev), nrow = nrow(ev))
  for (i in 1:ncol(ev))        # Using R's built-in poisson generator.
    pr[,i] <- rnorm(length(ev[,i]), mean = ev[,i], sigma = sd(sigma2[i]))
  qi <- list(ev = ev, pr = pr)
  qi.name <- list(ev = "Expected Values: E(Y|X)",
                  pr = "Predicted Values: Y|X")
  if (!is.null(x1)){
    ev1 <- par %*% t(x1)
    qi$fd <- ev1 - ev
    qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(par)), nrow = nrow(par), byrow = TRUE)
    tmp.ev <- yvar - qi$ev
    tmp.pr <- yvar - qi$pr
    qi$ate.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(par))
    qi$ate.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(par))
    qi.name$ate.ev <- "Average Treatment Effect: Y - EV"
    qi.name$ate.pr <- "Average Treatment Effect: Y - PR"
  }
  list(qi=qi, qi.name=qi.name)
}

data(macro)

user.prompt()

z.out <- zelig(unem ~ gdp + capmob + trade, model = "normal.regression", 
data = macro)
x.out <- setx(z.out)
s.out <- setx(z.out, x = x.out) 
