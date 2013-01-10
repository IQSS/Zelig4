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
#' @author Matt Owen \email{mowen@@iq.harvard.edu}, Kosuke Imai, Olivia Lau, and
#' Gary King 
#' Maintainer: Matt Owen \email{mowen@@iq.harvard.edu}
#' @keywords package
zelig <- function (formula, model, data, ..., by=NULL, cite=T) {

  # Yea this forever
  model.warnings(model)

  # Split data.frame
  if (!missing(by)) {

    if (length(by) > 1) {
      warning("by cannot have length greater than 1")
      by <- NULL
    }

    if (!is.data.frame(data))
      warning("")


    else if (any(by %in% all.vars(formula))) {
      warning("by cannot list contain a variable from the model's formula")
      by <- NULL
    }

    else
      data <- divide(data, by)
  }

  # Almost equivalient to:
  #   data <- multi.dataset(data)
  # 
  # but we want to keep the name of the original data object as our title (sometimes).
  data <- eval(call("multi.dataset", substitute(data)))

  # 
  Call <- match.call()

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

  # set up list
  res <- NULL
  old.style.oop <- TRUE

  # Call make.mi symbolically so that the function can implicitly label
  # data.frame's from context. For example, the expression:
  #   mi(turnout[1:1000, ], )
  # will contain a data.frame labeled:
  #   turnout[1:1000, ]
  # m <- eval(call("multi.dataset", substitute(data), by=by))

  # Ensure certain values remain consistent between any object on this list
  # by giving them all a pointer to the same environment object which contains
  # a few pieces of information
  state <- new.env()

  # Begin constructing zelig object
  object <- list()

  # Create zelig2* function
  zelig2 <- paste("zelig2", as.character(model), sep="")
  zelig2 <- get(zelig2, mode="function")

  # Get package name. This is useful for writing methods that apply to all
  # models within a particular software package
  package.name <- getPackageName(environment(zelig2), FALSE)

  # repeat
  for (key in names(data)) {
    d.f <- data[[key]]
    label <- key


    # catch end-of-list error
    if (is.null(d.f))
      next

    # print(formals(zelig2))
    zclist <- zelig2(formula, ..., data=d.f)

    if (!inherits(zclist, "z")) {
      # list of parameters to be ignored by external models IF not in
      # zelig2-return value
      remove <- c("model", "by", "cite", "...")

      # construct the call object
      zelig.call <- call("zelig.call", as.name("zclist"), as.name("remove"))
      zelig.env <- new.env()
      assign("zclist", zclist, zelig.env)
      assign("remove", remove, zelig.env)


      res.call <- zelig.call(Call, zclist, remove)
      new.call <- res.call$call
      env <- res.call$envir
    }
    else if (inherits(zclist, "z")) {
      new.call <- zclist$call
      env <- zclist$env
    }
    else {
      warning("zelig2 function is returning an invalid type of object")
    }

    attach(env)
    attach(d.f)



    tryCatch(
      {
        new.res <- eval(new.call, env)
      },
      error = function (e) {
        warning("There was an error fitting this statistical model.")
        new.res <- NULL
      }
      )

    detach(d.f)
    detach(env)

    # Apply first hook if it exists
    if (!is.null(zclist$.hook)) {
      zclist$.hook <- get(zclist$.hook, mode='function')
      new.res <- zclist$.hook(new.res, new.call, match.call(), ...)
    }

    # Determine whether this is an S4 object
    old.style.oop <- ! isS4(new.res)


    # This is the only "obj" assignment that matters
    obj <- makeZeligObject(new.res,
                           model,
                           new.call, match.call(),
                           d.f, label,
                           env,
                           package.name = package.name
                           )

    # Specify the appropriate class

    # Attach shared environment as an attribtute
    attr(obj, 'state') <- state

    # Add to list of results
    object[[label]] <- obj
  }

  if (missing(by) && is.data.frame(data)) {
    object <- object[[1]]
  }
  else {
    attr(object, 'state') <- state
    class(object) <- c(model, paste(model, 'mi', sep='-'), "MI")
  }

  # This used to be important, but we have API work-arounds now
  # methods.env <- if(old.style.oop)
  #   .RegisterMethodsS3(c("terms", register(obj)))
  # else
  #   .RegisterMethodsS4(c("terms", register(obj)))

  # Update the shared environment
  assign('old-formula', formula, state)
  assign('args', list(...), state)
  assign('parent', parent.frame(), state)
  assign('call', match.call(), state)
  assign('by', by, state)
  # assign('methods', methods.env, state)
  assign('methods', NULL, state)
  assign('model', model, state)


  # The below line should probably remain commented out
  # assign('mi', m, state)

  # Display citation information
  if (cite) {
    described <- describe(object)
    descr <- description(
                         authors = described$authors,
                         year  = described$description,
                         text  = described$text,
                         url   = described$url,
                         model = model
                         )
    cat("\n\n", cite(descr), "\n")
  }

  object
}





#' Make an Individual Zelig Object
#'
#' Returns a ``zelig'' object with the proper specifications
#' @param object a fitted statistical model
#' @param model a character-string specifying the name of the model
#' @param call The call that produced the fitted model
#' @param zelig_call The call made to the original zelig function
#' @param data the data.frame used to fit the model
#' @param label a character-string or symbol used as a human-readable label for
#' the data-set
#' @param env an environment variable that contains all variables to evaluate
#' the call ``zelig_call''
#' @param package.name a character-string specifyign the name of the package
#' that is the source of the model used to fit this object
#' @return A ``zelig'' object
makeZeligObject <- function (object,
                             model,
                             call,
                             zelig_call,
                             data,
                             label,
                             env,
                             package.name = NULL
                             ) {
  # This is a set of variables that will be visible to the following methods:
  # param, bootstrap, qi
  implied.variables <- new.env()

  # The fitted model
  assign(".fitted", object, implied.variables)

  # The name of the model
  assign(".model", model, implied.variables)

  # The call to the model-fitting function
  assign(".call", call, implied.variables)

  # The environment used to evaluate the model-fitting functino
  assign(".env", env, implied.variables)

  # Create list-object
  self <- list(
               result = object,
               formula = formula(object),
               zelig.call = zelig_call,
               name  = model,
               label = label,
               env  = env,
               call = call,
               data = data,
               S4   = isS4(object),
               method.env = implied.variables,
               package.name = package.name
               )

  # Specify as a ``zelig'' object
  class(self) <- c("zelig", model)

  # Return 
  self
}
