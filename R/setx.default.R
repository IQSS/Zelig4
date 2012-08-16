#' Set explanatory variables
#'
#' Set explanatory variables
#' @usage \method{setx}{default}(obj, fn=NULL, data=NULL, cond=FALSE, ...)
#' @S3method setx default
#' @param obj a 'zelig' object
#' @param fn a list of key-value pairs specifying which function apply to
#'           columns of the keys data-types
#' @param data a data.frame
#' @param cond ignored
#' @param ... parameters specifying what to explicitly set each column as. This
#'            is used to produce counterfactuals
#' @return a 'setx' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}, Kosuke Imai, and Olivia Lau 
setx.default <- function(obj, fn=NULL, data=NULL, cond=FALSE, ...) {

  # Warnings and errors
  if (!missing(cond))
    warning('"cond" is not currently supported by this version of Zelig')

  # Get formula used for the call to the model
  form <- formula(obj)

  # Parsed formula. This is an intermediate for used for processin design
  # matrices, etc.
  parsed.formula <- parseFormula(form, data)

  # If data.frame is not explicitly set, use the one from the Zelig call
  if (is.null(data))
    data <- obj$data

  # Create a variable to hold the values of the dot parameters
  dots <- list()

  # Get the dots as a set of expressions
  symbolic.dots <- match.call(expand.dots = FALSE)[["..."]]

  # Assign values to the dot parameters
  for (key in names(symbolic.dots)) {
    result <- with(data, eval(symbolic.dots[[key]]))
    dots[[key]] <- result
  }

  # Extract information about terms
  # Note: the functions 'getPredictorTerms' and 'getOutcomeTerms' are in need
  # of a rewrite. At the moment, they are pretty kludgey (written by Matt O.).
  vars.obj <- getPredictorTerms(form)
  not.vars <- getResponseTerms(form)

  # Default the environment to the parent
  env.obj <- parent.frame()

  # explanatory variables
  explan.obj <- Filter(function (x) x %in% vars.obj, names(dots))

  # defaults for fn
  if (missing(fn) || !is.list(fn))
    # set fn to appropriate values, if NULL
    fn <- list(numeric = mean,
               ordered = Median,
               other   = Mode
               )

  # res
  res <- list()

  # compute values
  # if fn[[mode(data(, key))]] exists,
  # then use that function to compute result
  for (key in all.vars(form[[3]])) {
    # skip values that are explicitly set
    if (key %in% names(dots) || key %in% not.vars)
      next

    m <- class(data[,key])[[1]]

    # Match the class-type with the correct function to call
    if (m %in% names(fn))
      res[[key]] <- fn[[m]](data[ ,key])

    # If it is a numeric, then we just evaluate it like a numeric
    else if (is.numeric(data[,key]))
      res[[key]] <- fn$numeric(data[ ,key])

    # If it's ordered, then we take the median, because that's the best we got
    else if (is.ordered(data[,key]))
      res[[key]] <- fn$ordered(data[ ,key])

    # Otherwise we take the mode, because that always kinda makes sense.
    else
      res[[key]] <- fn$other(data[ ,key])
  }

  # Add explicitly set values
  for (key in names(symbolic.dots)) {
    if (! key %in% colnames(data)) {
      warning("`", key,
              "` is not an column in the data-set, and will be ignored")
      next
    }

    res[[key]] <- if (is.factor(data[,key])) {
      factor(dots[[key]], levels=levels(data[,key]))
    }
    else
      dots[[key]]
  }

  # 
  res <- do.call("mix", res)

  # A list containing paired design matrices and their corresponding data.frame's
  frames.and.designs <- list()

  # Iterate through all the results
  for (k in 1:length(res)) {
    #
    label <- paste(names(res[[k]]), "=", res[[k]], sep="", collapse=", ")

    # Get specified explanatory variables
    specified <- res[[k]]

    # Construct data-frame
    d <- constructDataFrame(data, specified)

    # Construct model/design matrix
    m <- constructDesignMatrix(d, parsed.formula)

    # Model matrix, as a data.frame
    dat <- tryCatch(as.data.frame(m), error = function (e) NA)

    frames.and.designs[[label]] <- list(
      label = label,
      data.frame = d,
      model.matrix = m,
      as.data.frame = dat
      )
  }


  # Phonetically... setx's
  setexes <- list()

  for (key in names(frames.and.designs)) {
    mod <- frames.and.designs[[key]]$model.matrix
    d <- frames.and.designs[[key]]$data.frame
    dat <- frames.and.designs[[key]]$as.data.frame
    specified <- res[[k]]

    setexes[[key]] <- list(
      name   = obj$name,
      call   = match.call(),
      formula= form,
      matrix = mod,
      updated = d,
      data   = dat,
      values = specified,
      fn     = fn,
      cond   = cond,
      new.data = data,
      special.parameters = dots,
      symbolic.parameters = symbolic.dots,
      label = obj$label,
      explan = vars.obj,
      pred   = not.vars,
      package.name = obj$package.name
    )
    attr(setexes[[key]], "pooled") <- F
    class(setexes[[key]]) <- c(obj$name, "setx")
  }

  if (length(setexes) == 1) {
    attr(setexes, "pooled") <- FALSE
    setexes <- setexes[[1]]
    class(setexes) <- c(obj$name, "setx")
  }
  else {
    attr(setexes, "pooled") <- TRUE
    class(setexes) <- c(obj$name, "pooled.setx", "setx")
  }

  # Return
  setexes
}


#' Construct Data Frame
#' Construct and return a tiny (single-row) data-frame from a larger data-frame,
#' a list of specified values, and a formula
#' @param data a ``data.frame'' that will be used to create a small design matrix
#' @param specified a list with key-value pairs that will be used to explicitly
#' set several values
#' @return a ``data.frame'' containing a single row
constructDataFrame <- function (data, specified) {
  # Make a tiny data-frame with all the necessary columns
  d <- data[1,]

  # Give the computed values to those entries
  for (key in names(specified)) {
    val <- specified[[key]]

    if (is.factor(val) || !(is.numeric(val) || is.ordered(val)))
      val <- factor(val, levels=levels(data[,key]))

    d[,key] <- val
  }

  # Return tiny data-frame
  d
}

#' Construct Design Matrix from
#' Construct and return a design matrix based on a tiny data-frame (single-row).
#' @param data a ``data.frame'' (preferably single-rowed) that will be used to
#' create a small design matrix
#' @param formula a formula, whose predictor variables will be used to create a
#' design matrix
#' @return a design (model) matrix
constructDesignMatrix <- function (data, formula) {
  tryCatch(
           # Attempt to generate the design matrix of the formula
           model.matrix(formula, data), 

           # If there is a warning... probably do nothing
           # warning = function (w) w,

           # If there is an error, warn the user and specify the design
           # matrix as NA
           error = function (e) {
             NA
           }
           )
}
