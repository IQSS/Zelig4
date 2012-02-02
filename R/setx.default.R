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
  # Expand the dots
  dots <- list(...)

  # init important objects  
  form <- formula(obj)

  # Parsed formula. This is an intermediate for used for processing
  # design matrices, etc.
  parsed.formula <- parseFormula(form)


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
  for (key in all.vars(form[[3]])) {
    # skip values that are explicitly set
    if (key %in% names(dots) || key %in% not.vars)
      next

    m <- class(data[,key])[[1]]

    # match the class-type with the correct
    # function to call
    if (m %in% names(fn))
      res[[key]] <- fn[[m]](data[,key])

    else if (is.numeric(data[,key]))
      res[[key]] <- fn$numeric(data[,key])

    else if (is.ordered(data[,key]))
      res[[key]] <- fn$ordered(data[,key])

    else {
      res[[key]] <- fn$other(data[,key])
    }
  }


  # add explicitly set values
  for (key in names(dots)) {
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

  # Note that model.matrix.parsedFormula is called here.
  mod <- model.matrix(parsed.formula, d)
  dat <- as.data.frame(mod)
  rownames(mod) <- NULL

  # This space here should be reserved for manipulating interaction variables
  #
  #
  #

  # build the setx object
  sx <- list(
             name   = obj$name,
             call   = match.call(),
             formula= form,
             matrix = mod,
             updated = d,
             data   = dat,
             values = res,
             fn     = fn,
             cond   = cond,
             new.data = data,
             special.parameters = list(...),
             label = obj$label,
             explan = vars.obj,
             pred   = not.vars
             )

  # set class and return
  class(sx) <- c(obj$name, "setx")
  sx
}
