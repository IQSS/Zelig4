#' Estimating a Statistical Model
#'
#' The zelig command estimates a variety of statistical
#' models.  Use \code{zelig} output with \code{setx} and \code{sim} to compute
#' quantities of interest, such as predicted probabilities, expected values, and
#' first differences, along with the associated measures of uncertainty
#' (standard errors and confidence intervals).
#'
#' @param formula a symbolic representation of the model to be
#'   estimated, in the form \code{y \~\, x1 + x2}, where \code{y} is the
#'   dependent variable and \code{x1} and \code{x2} are the explanatory
#'   variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
#'   same dataset.  (You may include more than two explanatory variables,
#'   of course.)  The \code{+} symbol means ``inclusion'' not
#'   ``addition.''  You may also include interaction terms and main
#'   effects in the form \code{x1*x2} without computing them in prior
#'   steps; \code{I(x1*x2)} to include only the interaction term and
#'   exclude the main effects; and quadratic terms in the form
#'   \code{I(x1^2)}
#' @param model the name of a statistical model.
#'   Type \code{help.zelig("models")} to see a list of currently supported
#'   models
#' @param data the name of a data frame containing the variables
#'   referenced in the formula, or a list of multiply imputed data frames
#'   each having the same variable names and row numbers (created by
#'   \code{mi}) 
#' @param ... additional arguments passed to \code{zelig},
#'   depending on the model to be estimated
#' @param by a factor variable contained in \code{data}.  Zelig will subset
#'   the data frame based on the levels in the \code{by} variable, and
#'   estimate a model for each subset.  This a particularly powerful option
#'   which will allow you to save a considerable amount of effort.  For
#'   example, to run the same model on all fifty states, you could type:
#'   \code{z.out <- zelig(y ~ x1 + x2, data = mydata, model = "ls", by = "state")}
#'   You may also use \code{by} to run models using MatchIt subclass
#' @param cite If is set to "TRUE" (default), the model citation will be
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{z.out\$coefficients}.  See the specific models listed above
#'   for additional output values, or simply type \code{names(z.out)}.  
#'
#' @name zelig
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}, Kosuke Imai, Olivia Lau, and Gary King 
#'         Maintainer: Matt Owen \email{monwe@@iq.harvard.edu}
#' @keywords package
zelig <- function (formula, model, data, ..., by=NULL, cite=T) {
  Call <- match.call()

  if (!missing(by)) {
    if (any(by %in% all.vars(formula))) {
      warning("by cannot list contain a variable from the model's formula")
      by <- NULL
    }

    if (length(by) > 1) {
      warning("by cannot have length greater than 1")
      by <- NULL
    }
  }

  # expand dot arguments
  dots <- list()

  # get non-dot arguments in a general fashion
  notdots <- as.list(match.call(expand.dots=F)[-1])
  notdots[["..."]] <- NULL

  # only get the non-dot arguments
  # that do not exist in the dot arguments
  names.notdots <- Filter(function(x) !x%in%names(dots), names(notdots))
  notdots <- notdots[names.notdots]

  # build parameter list (including optional parameters)
  params <- c(dots, notdots)


  # construct model object
  class(model) <- model

  # set up list
  res <- NULL
  old.style.oop <- TRUE

  # create a data.frame iterator
  m <- mi(data, by=by)
  m <- reset(m)

  # initialize variables for loop
  k <- 1
  res <- list()
  res.env <- list()
  frames <- list()

  # repeat
  repeat {
    # get the next data.frame
    d.f <- try(nextElem(m), silent=TRUE)

    # catch end-of-list error
    if (inherits(d.f, "try-error")) {
      break
    }

    # create zelig2* function
    zelig2 <- paste("zelig2", as.character(model), sep="")
    zelig2 <- get(zelig2, mode="function")

    # call zelig2* function
    zclist <- zelig2(formula, ..., data=d.f)

    # list of parameters to be ignored by external models
    remove <- c("model", "by", "cite", "...")

    # construct the call object
    res.call <- zelig.call(Call, zclist, remove)
    new.call <- res.call$call

    env <- res.call$envir
    
    attach(env)
    new.call$weights <- NULL

    if (exists(".debug") && .debug) {
      message("External Call = ")
      print(new.call)
    }

    new.res <- eval(new.call)
    detach('env')

    # apply first hook if it exists
    if (!is.null(zclist$.hook)) {
      zclist$.hook <- get(zclist$.hook, mode='function')
      new.res <- zclist$.hook(new.res, new.call, match.call(), ...)
    }

    # test
    old.style.oop <- ! isS4(new.res)

    # append to list
    res[[k]] <- new.res
    res.env[[k]] <- env
    frames[[k]] <- d.f

    k <- k+1
  }

  key.labels <- get.mi.labels(m)

  names(res) <- key.labels
  names(res.env) <- key.labels
  names(frames) <- key.labels

  big.list <- list()

  # appropriately name each entry
  # (because both should be in order)
  
  # this is kludge.  can we (I) clean this up?
  if (!inherits(data, "mi") && is.null(by)) {
    res <- res[[1]]
    res.env <- res.env[[1]]
    frames <- frames[[1]]
  }

  else {
    for (key in key.labels) {
      
      big.list[[key]] <- list(
                              name = as.character(model),
                              formula = formula,
                              result = res[[key]],
                              envir = res.env[[key]],
                              args = list(...),
                              data = frames[[key]],
                              call = match.call(),
                              by = by,
                              mi = NULL,
                              func = zclist[[1]],
                              levels = NULL,
                              S4 = !old.style.oop,
                              parent = parent.frame(),
                              zc = zclist,
                              list = NULL
                              )
      class(big.list[[key]]) <- c('zelig', model)
    }

  }

  # run clean-up hooks on every result
  # ...


  # build zelig object
  z <- list(name    = as.character(model),
            formula = formula,
            result  = res,
            envir   = res.env,
            args    = list(...),
            data    = data,
            call    = match.call(),
            by      = by,
            mi      = reset(m),
            func    = zclist[[1]],
            levels  = m$levels,
            S4      = !old.style.oop,
            parent  = parent.frame(),
            zc = zclist,
            list = big.list
            )

  # always attach the model name,
  # so that developers can overload
  class(z) <- c("zelig", model)

  # ...
  z$function.space <- if(old.style.oop)
    .RegisterMethodsS3(c("terms", register(z)))
  else
    .RegisterMethodsS4(c("terms", register(z)))

  # prepend "MI" class to sets of results
  if (is.list(z$result) && length(m) > 1)
    class(z) <- c("MI", class(z))
  else if (inherits(data, "mi"))
    class(z) <- c("MI", class(z))

  
  # citation
  if (cite) {
    #
    described <- describe(z)

    #
    descr <- description(
                         authors = described$authors,
                         year  = described$description,
                         text  = described$text,
                         url   = described$url,
                         model = model
                         )

    cat("\n\n")
    cat(cite(descr), "\n")
  }

  z
}
