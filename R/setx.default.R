#' Set explanatory variables
#' @S3method setx default
#' @param obj a 'zelig' object
#' @param fn a list of key-value pairs specifying which function apply to
#'           columns of the keys data-types
#' @param data a data.frame
#' @param cond ignored
#' @param ... parameters specifying what to explicitly set each column as. This
#'            is used to produce counterfactuals
#' @return a 'setx' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}, Kosuke Imai, and Olivia Lau 
setx.default <- function(obj, fn=NULL, data=NULL, cond=FALSE, ...) {
  # Expand the dots
  dots <- list(...)

  # init important objects  
  form <- formula(obj)

  # Extract information about terms
  # Note: the functions 'getPredictorTerms' and 'getOutcomeTerms' are in need
  # of a rewrite. At the moment, they are pretty kludgey (written by Matt O.).
  vars.obj <- getPredictorTerms(form)
  not.vars <- getResponseTerms(form)

  # Default the environment to NULL (Global)
  env.obj <- NULL

  # explanatory variables
  explan.obj <- Filter(function (x) x %in% vars.obj, names(dots))

  # fix objects, if some don't come through correctly
  if (is.null(env.obj))
    env.obj <- parent.frame()

  # defaults for fn
  if (missing(fn) || !is.list(fn))
    # set fn to appropriate values, if NULL
    fn <- list(numeric = mean,
               ordered = Median,
               other   = Mode
               )

  # get data-frame
  if (is.null(data))
    data <- obj$data

  # res
  res <- list()

  # compute values
  # if fn[[mode(data(, key))]] exists,
  # then use that function to compute result
  for (key in vars.obj) {
    # skip values that are explicitly set
    if (key %in% names(dots) || key %in% not.vars)
      next

    m <- class(data[,key])

    # match the class-type with the correct
    # function to call
    if (m %in% names(fn))
      res[[key]] <- fn[[m]](data[,key])

    else if (is.numeric(data[,key]))
      res[[key]] <- fn$numeric(data[,key])

    else if (is.ordered(data[,key]))
      res[[key]] <- fn$ordered(data[,key])

    else
      res[[key]] <- fn$other(data[,key])
  }

  # add explicitly set values
  for (key in names(dots)) {
    if (! key %in% colnames(data)) {
      warning(key, "is not an column in the data-set, and will be ignored")
      next
    }

    res[[key]] <- if (is.factor(data[,key])) {
      factor(dots[[key]], levels=levels(data[,key]))
    }
    else
      dots[[key]]
  }


  # make a tiny data-frame with
  # all the necessary columns
  d <- data[1,]

  # give the computed values to those entries
  for (key in names(res)) {
    val <- res[[key]]

    if (is.factor(val) || !(is.numeric(val) || is.ordered(val)))
      val <- factor(val, levels=levels(data[,key]))

    d[,key] <- val
  }

  # build the model matrix
  # this is a cleaner but still hackneyed
  # of the data.frame casting that's done
  # in the original setx

  # we should always be able to guarantee that there is a foruma, so this
  # model.matrix is always available, but it is not always relevant
  mod <- model.matrix(form, data = d)
  mod <- as.data.frame(mod)
  rownames(mod) <- NULL


  # there is no guarantee that there is a terms object. If there isn't, then
  # we should just ignore it and make that style data.frame unavaiable
  terms <- tryCatch(terms(obj), error = function (e) NULL)

  if (is.null(terms))
    DataFrame <- NULL

  else {
    ModelFrame <- model.matrix(terms(obj), d)
    DataFrame <- as.data.frame(ModelFrame, row.names = NULL)
  }

  # build the setx object
  sx <- list(
             name   = obj$name,
             call   = match.call(),
             formula= form,
             matrix = mod,
             data.frame = DataFrame,
             values = res,
             fn     = fn,
             cond   = cond,
             new.data = data,
             updated  = d,
             special.parameters = list(...),
             label = obj$label,
             explan = vars.obj,
             pred   = not.vars
             )

  # set class and return
  class(sx) <- c(obj$name, "setx")
  sx
}


# Get
getResponseTerms <- function (x, ...) {
  # The following functions are unsafe for general input, so they are being
  # kept as nested functions.

  # Extract "outcome" terms from a formula
  # @param x a formula
  # @param ... ignored parameters
  # @return a character-vector specifying the predictor terms
  # @author Matt Owen
  extractFromFormula <- function (form, ...) {
    if (length(form) == 3)
      as.character(all.vars(form[[2]]))

    else if (length(form) == 2)
      ""

    else {
      warning("Formula of zelig object must have a length of 2 or 3")
      NA
    }
  }

  # Extract "outcome" terms from a list of formulae
  # @param x a list
  # @param ... ignored parameters
  # @return a character-vector specifying the 
  # @author Matt Owen
  extractFromList <- function (x, ...) {
    unlist(Map(extractFromFormula, x))
  }

  # Beginning of work for function
  if (is.list(x))
    extractFromList(x)

  else if ("formula" %in% class(x))
    extractFromFormula(x)

  else {
    warning("The model formula must either ",
            "be a list of formula to work properly")
    NA
  }
}


#
getPredictorTerms <- function (x, ...) {
  # The following functions are unsafe for general input, so they are being
  # kept as nested functions.

  # Extract "predictor" terms from a formula
  # @param x a formula
  # @param ... ignored parameters
  # @return a character-vector specifying the 
  # @author Matt Owen
  extractFromFormula <- function (form, ...) {
    if (length(form) == 3)
      as.character(all.vars(form[[2]]))
 
    else if (length(form) == 2)
      as.character(all.vars(form[[1]]))

    else {
      warning("Formula of zelig object must have a length of 2 or 3")
      NA
    }
  }

  # Extract "predictor" terms from a list of formulae
  # @param x a list
  # @param ... ignored parameters
  # @return a character-vector specifying the 
  # @author Matt Owen
  extractFromList <- function (x, ...) {
    unlist(Map(extractFromFormula, x))
  }

  # Beginning of work for function
  if (is.list(x))
    extractFromList(x)

  else if ("formula" %in% class(x))
    extractFromFormula(x)

  else {
    warning("The model formula must either ",
            "be a list of formula to work properly")
    NA
  }
}

