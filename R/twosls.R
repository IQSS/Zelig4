#' Interface between the Zelig Model twosls and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2twosls <- function (formula, ..., data) {

  loadDependencies(systemfit)

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

#' @S3method param twosls
param.twosls <- function(obj, num=1000, ...) {

  # Produce a vector of all terms
  big.coef <- coef(obj)

  # Produce a pretty sparse matrix containing 3 vcov matrices.
  #
  # Note that this matrix will give a value of zero to any invalid row-column
  # combination.
  # In particular, any terms that do not belong to the same equation will have
  # a zero value.
  big.vcov <- vcov(obj)

  # This is a complete list of the terms. This is largely ignored, aside from
  # the fact that we need a list of the formulae. In general, terms.multiple
  # produced a pretty unwieldy list of items.
  all.terms <- terms(obj)

  # This list stores the results
  simulations.list <- list()

  # Iterate through the set of terms, and simulate each list separately.
  for (key in names(all.terms)) {

    # Extract the terms for an individual model.
    eq.terms <- terms(all.terms[[key]])

    # Extract the labels for the terms
    eq.term.labels <- attr(eq.terms, "term.labels")

    # Add the labeled for the intercept column, if it should exist
    if (attr(eq.terms, "intercept"))
      eq.term.labels <- c("(Intercept)", eq.term.labels)

    # Format the title, this should look like:
    #   <list-item-name>_<term-label>
    #
    # So for the list: list(mu1 = y ~ x + sin(x))
    # We get:
    #   "mu1_(Intercept)" "mu1_x" "mu1_sin(x)"
    entries <- paste(key, eq.term.labels, sep = "_")

    # Extract the mean-value of this term (from the lumped-toegether vector)
    eq.coef <- big.coef[entries]

    # Extract the vcov matrix of this term (from the lumped-together matrix)
    eq.vcov <- big.vcov[entries, entries]

    # Simulate the parameters
    eq.simulations <- mvrnorm(num, eq.coef, eq.vcov)

    # Name the columns
    colnames(eq.simulations) <- eq.term.labels

    # Add to the list
    simulations.list[[key]] <- eq.simulations

  }


  # Return.
  list(
       coef = simulations.list,
       linkinv = NULL
       )
}

#' @S3method qi twosls
qi.twosls <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  # Compute the expected value of multistage LS methods
  compute.ev <- function (obj, x, param) {
    #
    if (is.null(x) || is.na(x)) {
      return(NA)
    }

    # If 'x' has too many rows, there will currently be errors. This is an issue
    # in Zelig-core
    if (nrow(x$matrix) > 1) {
      warning("This package does not currently support pooled results.")
      x <- x[1, ]
    }

    # Begin regular function
    terms <- terms(obj)

    # :q
    coef.list <- coef(param)

    # Hold Results
    eta <- list()

    #
    for (key in names(coef.list)) {
      #
      coef <- coef.list[[key]]
      # print(colnames(coef))
      small.x <- as.matrix(x$matrix[, colnames(coef)])
      #
      eta[[key]] <- coef %*% (small.x)
    }


    # Convert list into a matrix
    eta <- Reduce(function (x, y) cbind(x, y), eta)
    colnames(eta) <- names(terms)

    eta
  }

  ev1 <- compute.ev(obj, x, param)
  ev2 <- compute.ev(obj, x1, param)
  fd <- ev2 - ev1

  # Name each column after the associated equation

  # Return the results
  list(
       "Expected Value: E(Y|X)" = ev1,
       "Expected Value (for X1): E(Y|X1)" = ev2,
       "First Differences: E(Y|X1)-E(Y|X)" = ev2 - ev1
       )
}

#' @S3method describe twosls
describe.twosls <- function (...) {
  category <- "continuous"
  description  <- "Two Stage Least Squares"
  authors <- c("Ferdinand Alimadhi", "Ying Lu", "Elena Villalon")
  year <- 2007

  package <-list(
                 name = "systemfit",
		 version = "0.8"
		 )

  parameters <- list()
  parameters$mu <-list(
                       equations=c(2,Inf),
                       tagsAllowed=TRUE,
                       depVar=TRUE,
                       expVar=TRUE
                       )
  parameters$inst<-list(
                        equations=c(1,1),
                        tagsAllowed=FALSE,
                        depVar=FALSE,
                        expVar=TRUE
                        )
 
  list(category = category, authors = authors, year = year, description = description, package = package, parameters = parameters)
}

#' @S3method plot sim.twosls
plot.sim.twosls <- function (x, ...) {

  # Define locak function to plot a set of quantities of interest
  plotSet <- function (title) {
    for (col in colnames(qis[[title]])) {
      q <- qis[[title]][, col]
      plot(density(q), main = paste(col, title, sep=": "))
    }
  }

  # Code begins here

  qis <- as.list.qi(x$qi)
  qis <- Filter(function (y) any(!is.na(y)), qis)
  qis <- Filter(is.matrix, qis)


  max.cols <- max(unlist(Map(ncol, qis)))
  layout.matrix <- matrix(0, length(qis), max.cols)
  rownames(layout.matrix) <- names(qis)

  count <- 1

  for (title in names(qis)) {
    for (k in 1:ncol(qis[[title]])) {
      layout.matrix[title, k] <- count
      count <- count + 1
    }
  }

  layout(layout.matrix)

  for (key in names(qis)) {
    plotSet(key)
  }
}

#' @export
callsystemfit<-function(formula,data,method,inst=NULL,...){
  # Call systemfit..
  out <- systemfit(
                   data = data,
                   formula = formula,
                   method = method,
                   inst = inst,
                   ...
                   )

  # Assign class to formula, so that it is correctly parsed
  class(formula) <- c("multiple", "list")
  
  # Set the terms explicitly
  attr(out,"terms") <- terms(formula)

  # Set the class explicitly
  class(out) <- c("multiple", class(out))

  # Fin. Return the modified object
  return(out)
}

as.list.qi <- function (x, names = "") {
  class(x) <- "list"
  indices <- attr(x, ".index")
  attr(x, ".index") <- NULL
  rename.keys(x, indices, names(indices))
}

rename.keys <- function (x, keys, to, warn = TRUE) {
  all.names <- names(x)
  indices <- match(keys, all.names)

  if (any(is.na(indices)))
    stop("Keys contains values that are not in `x`")

  all.names[indices] <- to
  names(x) <- all.names

  x
}

