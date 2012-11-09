# THIS FILE CONTAINS PACKAGE HOOKS FOR ZELIG
# ------------------------------------------

# @...: nothing
# spill-over: output information about Zelig
.onAttach <- function(...) {

  package.name <- "Zelig"
  mylib <- dirname(system.file(package = package.name))
  ver <- packageDescription(package.name, lib.loc = mylib)$Version
  build.date <- packageDescription(package.name, lib.loc = mylib)$Date


  # build info
  packageStartupMessage("ZELIG (Versions ", ver, ", built: ", build.date, ")")

  # cat, for readability of the message text

  # Zelig info - do not exceed 80char/line
  packageStartupMessage("
+----------------------------------------------------------------+
|  Please refer to http://gking.harvard.edu/zelig for full       |
|  documentation or help.zelig() for help with commands and      |
|  models support by Zelig.                                      |
|                                                                |
|  Zelig project citations:                                      |
|    Kosuke Imai, Gary King, and Olivia Lau.  (2009).            |
|    ``Zelig: Everyone's Statistical Software,''                 |
|    http://gking.harvard.edu/zelig                              |
|   and                                                          |
|    Kosuke Imai, Gary King, and Olivia Lau. (2008).             |
|    ``Toward A Common Framework for Statistical Analysis        |
|    and Development,'' Journal of Computational and             |
|    Graphical Statistics, Vol. 17, No. 4 (December)             |
|    pp. 892-913.                                                |
|                                                                |
|   To cite individual Zelig models, please use the citation     |
|   format printed with each model run and in the documentation. |
+----------------------------------------------------------------+

")

  # Set class globally when the package is attached
  setClass("zelig", representation(), where=.GlobalEnv)
  setClass("MI", representation(), where=.GlobalEnv)
}

# @lis: list of characters
.RegisterMethodsS4 <- function(lis) {
  # create a new environment
  saved.env <- new.env()

  #
  for (fname in lis) {
    # Register Method S4
    # not: Register Methods S4
    # this is not recursive!
    .RegisterMethodS4(fname)
  }

  saved.env
}


# @name: character-string
.RegisterMethodS4 <- function(name) {
  res <- try(setMethod(name, signature="zelig",
                       where = .GlobalEnv,
                       definition=.NewZeligGenericS4(name)
                       ),
             silent=TRUE
             )

  res <- try(setMethod(name, signature="MI",
                       where = .GlobalEnv,
                       definition=.NewZeligMIGenericS4(name)
                       ),
             silent=TRUE
             )

  if (inherits(res, "try-error")) {
    FALSE
  }

  else {
    res
  }
}


# @name: name of the generic
# return: anonymous function
# **************************
# note that the following function changes the
# formal arguments to that of the original
# generic function.  this is to suppress most
# warnings that crop up while setting an S4 method
.NewZeligGenericS4 <- function(name) {
  # get generic
  fdef <- getGeneric(name)

  #
  fformals <- formals(fdef)
  stored.name <- name

  # 
  f <- function(object, ...) {
    # get list of parameters,
    # ommitting function name
    params <- as.list(sys.call())[-1]

    # get the result object from the zelig object
    params[[1]] <- eval.parent(params[[1]])$result

    # call the function on the fitted model
    do.call(name, params)
  }

  # assign formals, so that they match perfectly
  formals(f) <- fformals

  # return
  f
}

#
#
.NewZeligMIGenericS4 <- function(name) {
  # get generic
  fdef <- getGeneric(name)

  #
  fformals <- formals(fdef)
  stored.name <- name

  # define function
  f <- function(object, ...) {
    params <- as.list(sys.call())
    params[[1]] <- stored.name
    params[[2]] <- eval.parent(params[[2]])$result

    #
    do.call(Map, params)
  }

  formals(f) <- fformals

  # return
  f
}



# @object: a zelig object
.GetGenericsS4 <- function(object, envir=parent.frame()) {
  if (inherits(object$result, "list")) {
    .ListS4Generics(classes=class(object$result[[1]]), env=envir)
  }
  else 
    .ListS4Generics(classes=class(object$result), env=envir)
}


# @classes: classes
# @where: compatibility with showMethods
# @env: the environment to search for generics
# return: a character-vector of function names
# ********************************************
# this function searches .AllMTable within the namespace
# of the functions environment
.ListS4Generics <- function(classes=NULL, where=NULL,
                          env=topenv(parent.frame())) {
  # get list of all generic functions
  functions <- if (missing(where))
    getGenerics()
  else
    getGenerics(where)

  #
  matches <- c()
  functions <- as.character(functions)

  #
  for (f in functions) {
    fdef <- getGeneric(f)
    env <- environment(fdef)

    table <- tryCatch(get(".AllMTable", envir=env), error=function(e) NULL)

    if (is.null(table))
      next

    if (any(classes %in% ls(table)))
      matches <- append(matches, f)
  }

  # return
  flist <- c("zelig", "param", "as.parameters", "sim", "setx", "register", 'summary')
  matches[ ! matches %in% flist ]
}

#' Describe a Zelig Model
#'
#' @param model.name 
#' @param ... ignored parameters
#' @return a 'description' object containing citation information
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.ZeligDescribeModel <- function(model.name, ...) {
  # lie to zelig
  dummy.zelig <- "dummy"
  class(dummy.zelig) <- model.name

  # return as a description
  as.description(describe(dummy.zelig))
}

#' Get a Character-Vector of All Models with a 'zelig2' Function
#'
#' @note In order for a Zelig model to either execute correctly or be listed as
#'   a legal Zelig model, the function name must be prefixed with 'zelig2'.
#' @param zelig.only a boolean specifying whether we want to search only the 
#'   Zelig namespace
#' @return a character-vector of the Zelig models loaded on the user's machine
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
ZeligListModels <- function(zelig.only=FALSE) {
  results <- if (zelig.only)
    ls(pattern="^zelig2", envir=asNamespace("Zelig"))
  else
    apropos("^zelig2", mode="function")

  # substitute and return
  sub("^zelig2", "", results)
}

#' Get a Text-Block of Citation Information about a Zelig Model
#' 
#' @note This function is strictly used internally by Zelig
#' @param model.name the name of a Zelig model
#' @return a block of text giving a human readable (and APA compliant) block
#'   citation text
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.GetModelCitationTex <- function(model.name)
  cite(ZeligDescribeModel(model.name))

#' Produce a 'description' Object from the Name of a Model
#' @note The 'description' object is a list-style object containing citation
#'   information
#' @param model.name a character-string specifying a Zelig model
#' @return a 'description' object specified by the 'model.name' parameter. This
#'   object is created by executing the specified Zelig models' 'describe'
#'   function
#' @export
ZeligDescribeModel <- function(model.name) {
  dummy <-
    "I love baseball.  You know, it doesn't have to mean anything.
It's just very beautiful to watch."
  
  class(dummy) <- model.name

  # describe
  res <- describe(dummy)

  # add model name
  res$model <- model.name

  # return
  as.description(res)
}

#' Get a TeX-style Citation
#' @param model a character-string specifying the name of the Zelig model of which 
#'   to describe in TeX-style
#' @return a string to be rendered as part of a LaTeX-style document
#' @export
TexCite <- function(model) {
  # description object
  descr <- ZeligDescribeModel(model)

  # url
  url <- "http://gking.harvard.edu/zelig"

  # define title
  title <- if (is.null(descr$text))
    descr$model
  else
    paste(descr$model, ": ", descr$text, sep="")

  # quote title string
  title <- paste('"', title, '"', sep="")

  # construct string
  str <- paste(
               "{\bf To cite this model in Zelig:}",
               paste(descr$authors, descr$year, sep="."),
               paste(title, "in Kosuke Imai, Gary King and Olivia Lau,"),
               "\"Zelig: Everyone's Statistical Software,\"",
               url,
               sep = "\n"
               )
  str
}

#' Get a List of Categories for Describing Zelig Models
#' @note This feature is being deprecated, as original functionality with the
#'   Dataverse Project \url{thedata.org} is being reevaluated.
#' @return a list of character-string specifying legal category types (as the
#'   keys of the list) and their human-counterparts (as the values)
#' @export
.ZeligModelCategories <- function() {
  list(continuous  = "Models for Continuous Dependent Variables",
       dichotomous = "Models for Dichotomous Dependent Variables",
       ordinal     = "Models for Ordinal Dependent Variables",
       bounded     = "Models for Continous Bounded Dependent Variables",
       multinomial = "Multinomial Choice Models",
       count       = "Event Count Models",
       mixed       = "Models for Mixed Dependent Variables",
       ei          = "Ecological Inference Models"
       )
}

#' List the Titles of the Zelig Statistical Models
#' @return a list of manual titles for the Zelig software 
#' @export
ZeligListTitles <- function() {

  #
  models <- ZeligListModels()

  #
  lis <- list()

  #
  for (m in models)
    lis[[m]] <- ZeligDescribeModel(m)$text

  # turn into a vector with each entry having:
  #  model_name: model_description
  # e.g.
  #  probit: Probit Regression for Dichotomous Dependent Variables
  paste(names(lis), lis, sep=": ")
}

#' Whether an Arbitrary R-package has a Zelig2 Function within Its Namespace
#' @note This function is used primarily internally to determine whether a
#'   a package is contributing a function to the Zelig software suite
#' @param pkg a character-string representing a package name
#' @return whether the package contains any zelig2-functions
#' @export
has.zelig2 <- function(pkg) {
  env <- asNamespace(pkg)
  hits <- grep("^zelig2*", ls(envir=env))
  length(hits) > 0
}

#' Whether a Statistical Package Depends on the Zelig Software Suite
#' @note This function is used primarily internally to determine whether a
#'   a package is contributing a function to the Zelig software suite
#' @param package a character-string representing a package name
#' @return whether the package lists Zelig as a dependency in its DESCRIPTION
#' @export
depends.on.zelig <- function(package="") {
  zcomp <- packageDescription(package, fields="Depends")

  if (is.na(zcomp))
    return(FALSE)

  zcomp <- unlist(strsplit(zcomp, " *, *"))

  # "Zelig" %in% zcomp

  # pattern to match things leading with Zelig, some spaces, and a parenthesis ending
  # ex:
  #     Zelig
  #     Zelig (>= 3)
  #     Zelig      (blah blah)
  pattern <- "^Zelig *(?:\\(.*?\\))$"
  length(grep(pattern, zcomp)) != 0
}

#' Get a List of Packages Installed on the Current Machine that Depend on Zelig
#' @note This function is used primarily internally to determine whether a
#'   a package is contributing a function to the Zelig software suite
#' @return a character-vector of all zelig-dependent packages on the current
#'   machine
list.zelig.dependent.packages <- function() 
  Filter(depends.on.zelig, .packages(all.available=TRUE))

#' List Zelig Models Installed on the Current Machine
#' @note This list is not necessarily complete
#' @param with.namespace a boolean specifying whether 
#' @return list of all zelig models
list.zelig.models <- function(with.namespace=TRUE) {
  # list the zelig-dependent packages
  pkgs <- list.zelig.dependent.packages()

  # include the core package
  pkgs <- c("Zelig", pkgs)

  # initialize functions variable
  functions <- NULL

  # create a list of every zelig2 function
  for (pkg in pkgs) {
    # get all zelig2 functions, then get their model name
    models <- ls(pattern="^zelig2", envir=asNamespace(pkg))
    models <- sub("^zelig2", "", models)

    # add to results list
    functions[models] <- pkg
  }

  # return
  if (with.namespace)
    # with model-name as the key, and namespace as the value
    functions
  
  else
    # with just a list of models
    names(functions)
}
#' Store Named Values in an Newly Created Environment
#' @note This function is exclusively used internally by Zelig
#' @param params a list of parameters to create a Zelig Call
#' @return a list containing:
#           * parameters - a list of symbols
#           * envir - an environment where variables
#             contained in envir exist
.store.values <- function(params) {
  #
  e <- new.env()

  #
  named.params <- list()
  names <- names(params)

  #
  for (key in names) {

    # the following if-statements are meant to prevent the storage of
    # small call-function readable data-types. That is, these things
    # are very easily printable and have no need to be tucked away
    if (inherits(params[[key]], 'formula')) {
      named.params[[key]] <- params[[key]]
      next
    }

    if (is.character(params[[key]])) {
      named.params[[key]] <- params[[key]]
      next
    }

    if (is.numeric(params[[key]])) {
      named.params[[key]] <- params[[key]]
      next
    }

    if (inherits(params[[key]], "literal")) {
      named.params[[key]] <- params[[key]]$value
      next
    }


    # Everything below here gets processed
    # in the default manner
    # .............
    
    # prefix keyname if there is an overlap
    keyname <- .prefix(key, envir=e, prefix="stored")

    # store variable `keyname` in zelig environment
    assign(keyname, params[[key]], envir=e)

    # create a symbolic list
    named.params[[key]] <- as.name(keyname)
  }

  # return list
  list(params=named.params, envir=e)
}


#' Append a Prefix to a Character String
#' @note This function is exclusively used internally by Zelig
#' @param name a character-string specifying the name of a variable
#' @param envir an environment variable to search
#' @param prefix a character-string to prefix the string with
#'   this is applied until the name is unique
#' @param sep a character-string that separates prefix and name
.prefix <- function(name, envir, prefix="zelig", sep=".") {

  # check to make sure this is an environment variable
  if (!is.environment(envir)) {
    warning()
    envir <- globalenv()
  }

  # ensure some name is returned
  if (!is.character(c(name, prefix, sep))) {
    warning()
    name
  }

  else if (length(name) > 1 || length(prefix) > 1 || length(sep) > 1) {
    warning()
    name
  }

  else if (!nchar(name)) {
    warning()
    sep <- "."
  }

  else {
    while(exists(name, envir=envir))
      name <- paste(prefix, name, sep=sep)

    # return if nothing wonky happened
    name
  }
}


#' Attach Environment and Evaluate Expression
#'
#' Run in an insulated environment
#' @usage eval.in(...)
#' @param ... Two parameters: The first is an expression to evaluate, and the 
#' second the environment to evaluate the expression in
#' @return evaluation of ``..1'' within the environment ``..2''
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
eval.in <- .call <- function (...) {
  attach(..2)
  res <- suppressMessages((..1))
  detach(..2)
  res
}
.GetGenerics <- function(...) UseMethod(".GetGenerics")

# needs work
.GetGenerics.MI <- function(...) new.env()

# @zelig.object: a zelig object
# @envir:        namespace to search with 'ls'
# return:        a list of generic functions names to
#                to define for zelig
.GetGenerics.default <- function(zelig.object, envir=parent.frame()) {
  if (is.null(zelig.object$S4))
    stop(as.character(zelig.object$family[[1]]))
  else if (zelig.object$S4) 
    suppressWarnings(.GetGenericsS4(zelig.object, envir))
  else
    suppressWarnings(.GetGenericsS3(zelig.object, envir))
}

.GetGenericsS3 <- function(zelig.object, envir=parent.frame()) {
  #
  hash <- list()
  cls <- class(zelig.object$result)
  method.list <- as.character(unlist(mapply(methods, class=cls)))

  regex <- paste("(", paste(cls, collapse="|"), ")", sep="|")


  method.list <- gsub(regex, "", method.list)

  meth.list <- c()
  for (cl in c(class(zelig.object$result), "default")) {
    method.list <- as.character(methods(class=cl))
    method.list <- gsub(paste("\\.", cl, "$", sep=""), "", method.list)
    meth.list <- unique(c(meth.list, method.list))
  }

  # final list
  flist <- c("zelig", "param", "as.parameters", "sim", "setx", "register",
             'qi', 'summary')
  meth.list <- sort(unique(c(meth.list,
                             names(get(".knownS3Generics")))))

  meth.list[ ! meth.list %in% flist ]
}


# @name:  name of generic function
# return: a function that calls the given generic 
#         function on the result object
.NewZeligGenericS3 <- function(name) {
  # store name in this environment
  stored.name <- name

  # return function
  function (...) {
    # get list of parameters,
    # omitting function name
    params <- as.list(match.call())[-1]

    # get the result object from the zelig object
    params[[1]] <- ..1$result

    #attach(..1$envir)
    # call function on the fitted model
    eval.in(do.call(stored.name, params), ..1$envir)
    #detach(..1$envir)
  }
}


.NewZeligMIGenericS3 <- function(name) {
  # store name in this environment
  stored.name <- name

  # return function
  function (...) {
    # get list of parameters
    params <- as.list(match.call())

    # pass stored name as parameter
    # so that Map applies this to ...
    params[[1]] <- stored.name

    # get the result object from the zelig object
    params[[2]] <- ..1$result


    # call function on the fitted model
    do.call(Map, params)
  }
}


# @object: a zelig object
# @lis: a character-vector of names to register for zelig objects
.RegisterMethods <- function(lis)
  .RegisterMethodsS3(lis)



# @lis: 
# return: 
.RegisterMethodsS3 <- function(lis) {
  # error-catching & handling
  saved.environment <- new.env()

  # iterate through the list
  for (meth in lis) {
    fname <- paste(meth, "zelig", sep=".")
    # assign the relevant function in the
    # correct environment or namespace
    assign(fname, .NewZeligGenericS3(meth), envir = .GlobalEnv)

    # assign MI version
    fname <- paste(meth, "MI", sep=".")
    assign(fname, .NewZeligMIGenericS3(meth), .GlobalEnv)    
  }

  # return
  invisible(saved.environment)
}

# @f: a character-string specifying the function name
# @class: a character-string specifying the class name
.existsS3method <- function(f, class, ...)
  tryCatch(is.function(getS3method(f, class, ...)),
           error=function (e) FALSE
           )
# numerical_functions.R
# ---------------------
# contents:
#  * .nderiv
#  * .nr
#  * .NumInverse


# Numerical Derivative
#
# This method computes the numerical derivative at a point
# @param f function (differentiable)
# @param stencil number of points in stencil. This is currently ignored.
# @param h size of mesh
# @return anonymous function with the approximation
# @note single variable numerical derivative
.nderiv <- function(f, stencil=5, h=sqrt(.Machine$double.eps)) {
  # return approximated derivative function
  function (x) {
    # construct the 5-point mesh, middle point omitted
    # since it gets deleted anyway
    x.stencil <- rep(x, 4) + c(2, 1, -1, -2)*h

    # compute approximation
    sum(sapply(x.stencil, f) %*% c(-1, 8, -8, 1))/12/h
  }
}



# @F: function to invert
# @f: derivative of function, or NULL to use numerical approximation
# @x: initial guess
# @tol: error-tolerance
# @h: mesh size
# @max.iter: number of iterations to perform before giving up
# return: df(x_0)/dx
# **note: newton-rhapson for single variables
# **suggestion: replace with C code, otherwise won't be truly fast-enough
.nr <- function(F, f=NULL, x = 1, a = 0,
                tol      = sqrt(.Machine$double.eps),
                h        = sqrt(.Machine$double.eps),
                max.iter = 50) {
  # save function to prevent recursions
  saved.function <- F

  # rewrite function to solve for a
  if (!missing(a))
    F <- function(x) saved.function(x) - a
  
  # if NULL assign numerical derivative
  if (is.null(f))
    f <- .nderiv(F)

  #
  count <- 1

  #
  while (abs(F(x)) > tol && count <= max.iter) {
    # increment counter
    count <- count + 1

    # if derivative is zero, or near it
    # (otherwise we have issues with solutions where x=0)
    if (abs(f(x)) < 10^-8) {
      x <- x + runif(1, min=-1, max=1)
      next
    }

    # iterate
    x <- x - F(x)/f(x)
  }

  if (count > max.iter)
    warning("approximation failed to converge given specified tolerance")

  # return result
  x
}


# @F:
# @f:
# @x: initial guess
# @tol: 
# return: a functional form of the newton-rhapson approximation
.NumInverse <- function(F, f=NULL, x = 1,
                        tol      = (.Machine$double.eps)^.5,
                        h        = sqrt(.Machine$double.eps),
                        max.iter = 50) {
  function (a) {
    res <- c()

    # kludgey, but just a hold-over for now
    for (val in a) {
      val <- .nr(F=F, f=f, x=x, a=val, tol=tol, h=h, max.iter=max.iter)
      res <- c(res, val)
    }

    res
  }
}
# This file contains overloaded operators 
# However, developers - in general - should avoid the use of these features,
# and instead use iterators when dealing with multiple fitted models or
# quantities of interest.
# The methods primarily come up when defining 'summarize' and 'plot' functions


#' Extract a Value from a Fitted Model Object (Wrapped by Zelig)
#' @S3method "[[" zelig
#' @param z an object of type 'zelig'
#' @param slot a character-string specifying the slot to extract from the fitted
#'   model object
#' @param ... subsequent slots to extract from the fitted model object
#' @return contents of the specified slots
#' @author Matt Owen \emph{mowen@@iq.harvard.edu}
"[[.zelig" <- GetSlot.zelig

#' Extraction Operator for Quantities of Interest
#' This function is exclusively used internally by Zelig, and behaves in a very
#' fishy manner. \code{qi} objects maintain an internal list of indices which
#' are used to find the appropriate slot which holds a particular quantity of
#' interest.
#' When a \code{qi} object is defined, all the quantities of interest are
#' converted into acronyms, so that elements of the \code{qi} object can be
#' stored without a lengthy name containing spaces (since most qi's are
#' human-readable). As a result, these objects contain an \code{.index}
#' attribute which pairs every quantity of interest with its acronym. This
#' index is then used to extract (using the \code{$} operator) the appropriate
#' element of the list.
#' In short, it pairs the key "Expected Value" with the slot \code{ev}. This
#' allows that the following will always be true (in the mentioned example):
#'   \code{qi$ev == qi[["Expected Value"]]}
#' @note When possible, \code{qi} objects should be handled with iterators
#'   rather than list-style extraction operators.
#' @S3method "[[" qi
#' @param self the \code{qi} object
#' @param key a character-string specifying the title of the quantity of
#'   interest to extract.
#' @return if the quantity of interest exists, that entry. Otherwise,
#'   \code{NULL}
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
"[[.qi" <- function(self, key) {

  # Produce the index of titles of qi's
  index <- attr(self, ".index")

  # Find the 'short-name' matching
  qi.short.name <- index[[key]]

  if (is.null(qi.short.name))
    NULL
  else
    # if this title => key pair is found, invoke the "$" operator on the
    # shortname. In effect, this makes:
    #   qi[['Expected Value']]
    #
    # equivalent to:
    #   qi$ev
    do.call("$", list(self, qi.short.name))
}
#' Receiver Operator Characteristic Plots
#'
#' The 'rocplot' command generates a receiver operator characteristic plot to
#' compare the in-sample (default) or out-of-sample fit for two logit or probit
#' regressions.
#'
#' @usage
#' rocplot(y1, y2, fitted1, fitted2,
#' cutoff = seq(from=0, to=1, length=100), lty1="solid",
#' lty2="dashed", lwd1=par("lwd"), lwd2=par("lwd"),
#' col1=par("col"), col2=par("col"),
#' main="ROC Curve",
#' xlab = "Proportion of 1's Correctly Predicted",
#' ylab="Proportion of 0's Correctly Predicted",
#' plot = TRUE, 
#' ...
#' )
#'
#' @param y1 response variable for the first model
#' @param y2 response variable for the second model
#' @param fitted1 fitted values for the first model. These values may represent
#'   either the in-sample or out-of-sample fitted values
#' @param fitted2 fitted values for the second model
#' @param cutoff A vector of cut-off values between 0 and 1, at which to
#'   evaluate the proportion of 0s and 1s correctly predicted by the first and
#'   second model.  By default, this is 100 increments between 0 and 1
#'   inclusive
#' @param lty1 the line type of the first model (defaults to 'line')
#' @param lty2 the line type of the second model (defaults to 'dashed')
#' @param lwd1 the line width of the first model (defaults to 1)
#' @param lwd2 the line width of the second model (defaults to 1)
#' @param col1 the color of the first model (defaults to 'black')
#' @param col2 the color of the second model (defaults to 'black')
#' @param main a title for the plot (defaults to "ROC Curve")
#' @param xlab a label for the X-axis
#' @param ylab a lavel for the Y-axis
#' @param plot whether to generate a plot to the selected device
#' @param \dots additional parameters to be passed to the plot
#' @return if plot is TRUE, rocplot simply generates a plot. Otherwise, a list
#'   with the following is produced:
#'   \item{roc1}{a matrix containing a vector of x-coordinates and
#'     y-coordinates corresponding to the number of ones and zeros correctly
#'     predicted for the first model.}
#'   \item{roc2}{a matrix containing a vector of x-coordinates and
#'     y-coordinates corresponding to the number of ones and zeros correctly
#'     predicted for the second model.}
#'   \item{area1}{the area under the first ROC curve, calculated using
#'     Reimann sums.}
#'   \item{area2}{the area under the second ROC curve, calculated using
#'     Reimann sums.}
#' @export
#" @author Kosuke Imai and Olivia Lau
rocplot <- function(y1, y2, fitted1, fitted2,
                    cutoff = seq(from=0, to=1, length=100), lty1="solid",
                    lty2="dashed", lwd1=par("lwd"), lwd2=par("lwd"),
                    col1=par("col"), col2=par("col"),
                    main="ROC Curve",
                    xlab = "Proportion of 1's Correctly Predicted",
                    ylab="Proportion of 0's Correctly Predicted",
                    plot = TRUE, 
                    ...) {
  roc1 <- roc2 <- matrix(NA, nrow = length(cutoff), ncol = 2)
  colnames(roc1) <- colnames(roc2) <- c("ones", "zeros")
  for (i in 1:length(cutoff)) {
    roc1[i,1] <- mean(fitted1[y1==1] >= cutoff[i]) 
    roc2[i,1] <- mean(fitted2[y2==1] >= cutoff[i])
    roc1[i,2] <- mean(fitted1[y1==0] < cutoff[i])
    roc2[i,2] <- mean(fitted2[y2==0] < cutoff[i])
  }
  if (plot) {
    plot(0:1, 0:1, type = "n", xaxs = "i", yaxs = "i",
         main=main, xlab=xlab, ylab=ylab, ...)
    lines(roc1, lty = lty1, lwd = lwd1, col=col1)
    lines(roc2, lty = lty2, lwd = lwd2, col=col2)
    abline(1, -1, lty = "dotted")
  }
  else {
    area1 <- area2 <- array()
    for (i in 2:length(cutoff)) {
      area1[i-1] <- (roc1[i,2] - roc1[(i-1),2]) * roc1[i,1] 
      area2[i-1] <- (roc2[i,2] - roc2[(i-1),2]) * roc2[i,1] 
    }
    return(list(roc1 = roc1, 
                roc2 = roc2,
                area1 = sum(na.omit(area1)),
                area2 = sum(na.omit(area2))))
  }
}
#' Create Function Call
#'
#' 
#' @param Call a \code{call} object, typically specifying the original function
#'   call to \code{zelig}
#' @param zelig2 the return-value of the \code{zelig2} method
#' @param remove a list of character vectors specifying which parameters to
#'   ignore from the original call to \code{zelig}
#' @return a function call used to fit the statistical model
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig.call <- function(Call, zelig2, remove = NULL) {
  #
  envir <- new.env()

  # reserved words taken from the zelig2 method
  func <- as.name(zelig2$.function)
  hook <- zelig2$.hook

  # remove the reserved words
  zelig2$.function <- NULL
  zelig2$.hook <- NULL
  zelig2$.post <- NULL
  zelig2$.model.matrix <- NULL

  # make a list of the parameters to be passed to the external model
  args <- names(formals(as.character(func)))

  # remove certain parameters
   for (key in remove) {
     if (key %in% names(Call))
       Call[[key]] <- NULL
   }

  # remove invalid params
  for (key in names(Call[-1])) {
    if (! key %in% args)
      Call[[key]] <- NULL
  }



  # A static list of objects that do not printout well or should be stored
  # within a separate environment
  messy.objects <- c("data.frame", "function", 'matrix', "family", "function")
  neat.objects <- c("formula", "family")
  skip <- c()

  # Store values within 'messy.objects' within another environment, and give a 
  # pseudonym
  for (key in names(zelig2)) {
    obj <- zelig2[[key]]
    Class <- class(obj)
    first.class <- Class[1]

    if (is.object(obj)) {
      if (all(Class %in% neat.objects)) {
        Call[[key]] <- obj
      }
      else {
        Name <- store.object(obj, envir, ucfirst(first.class))
        Call[[key]] <- as.name(Name)
        skip <- c(skip, key)
      }
    }

    else if (is.function(obj)) {
      Name <- store.object(obj, envir, "Function")
      Call[[key]] <- as.name(Name)
      skip <- c(skip, key)
    }
    else if (is.atomic(obj) && length(obj) > 5) {
      Name <- store.object(obj, envir, paste(toupper(Class[1]), length(obj),
                                             sep=""))
      Call[[key]] <- as.name(Name)
      skip <- c(skip, key)
    }
    else if (is.list(obj) && length(obj) > 5) {
      Name <- store.object(obj, envir, paste("List", length(obj), sep=""))
      Call[[key]] <- as.name(Name)
      skip <- c(skip, key)
    }
    else {
      # this is a hack to prevent removal of elements if the value is NULL
      null.list <- list(NULL)
      names(null.list) <- key

      # the two statement are *slightly* different
      if (is.null(obj)) {
        Call <- as.call(append(as.list(Call), null.list))
      }
      else {
        Call[[key]] <- obj
      }
    }

  }

  # Guarantee all zelig2 names are included (including model, etc)
  for (key in names(zelig2)) {
    if (key %in% skip)
      next;

    if (!is.null(zelig2[[key]]))
      Call[[key]] <- zelig2[[key]]
    else {
      # Clear the entry. Don't worry. It's going to get re-added later in this
      # Else-block.
      Call[[key]] <- NULL

      # Create the NULL paramater
      dummylist <- list(NULL)
      names(dummylist) <- key

      # Cast as a list, so we can use append
      Call <- as.list(Call)

      # Append the entry
      Call <- as.call(append(Call, dummylist))
    }
  }


  # Change function value
  Call[[1]] <- func

  list(call=Call, envir=envir)
}

#' Store Object in Environment with a Fake Name
#'
#' This function takes the value of an object and stores it within a specified 
#' environment. This is similar to simply using the \code{assign} function, but
#' will not overwrite existing values in the specified environment. It
#' accomplishes this by appending a prefix to the name of the variable until
#' the name becomes unique.
#' @note This method does not correct invalid names. That is, there is no test
#'   to determine whether the submitted name is valid.
#' @param obj any object
#' @param envir an environment object, which will contain the object with the
#'   assigned name
#' @param name a character-string specifying the name that the object will be
#'   stored as in the specified environment
#' @param prefix a character string specifying the prefixes to append to names
#'   that already have matches in the destination environment
#' @return a character-string specifying the name of the object in the
#'   destination environment
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
store.object <- function (obj, envir, name=NULL, prefix=".") {

  variables <- ls(envir=envir)
  
  # ensure name is unique
  while (name %in% variables)
    name <- paste(prefix, name, sep="")

  assign(name, obj, envir)

  name
}

#' Uppercase First Letter of a String
#' 
#' This method sets the first character of a string to its uppercase,
#' sets all other characters to lowercase.
#' @param str a vector of charaqcter-strings
#' @return a vector of character strings
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
ucfirst <- function (str) {
  paste(
        toupper(substring(str, 1, 1)),
        tolower(substring(str, 2)),
        sep = ""
        )
}
#' Search for, Copy, and Customize Template for a Newly Create Zelig Package
#' This is used internally by \code{zelig.skeleton}
#' @param model a character-string specifying the name of the model
#' @param the file template to search for and copy
#' @param pkg a character-string specifying the name of the package
#' @param path a character-string specifying the base path to the package's u
#'  parent directory
#' @return This function is used for its side-effects.
.copy.templates <- function (model, pkg, path) {
  # path to R folder
  r.path <- file.path(path, pkg, 'R')

  # source files
  zelig2 <- system.file('templates', 'zelig2.R', package="Zelig")
  param <- system.file('templates', 'param.R', package="Zelig")
  qi <- system.file('templates', 'qi.R', package="Zelig")
  describe <- system.file('templates', 'describe.R', package="Zelig")

  # create R directory
  dir.create(r.path, showWarnings=FALSE)

  # destination files
  zelig2.dest <- file.path(r.path, paste('zelig2', model, '.R', sep=""))
  param.dest <- file.path(r.path, paste('param', model, 'R', sep="."))
  qi.dest <- file.path(r.path, paste('qi', model, 'R', sep="."))
  describe.dest <- file.path(r.path, paste('describe', model, 'R', sep="."))

  # create blank files
  file.create(zelig2.dest, param.dest, qi.dest)

  # substitute
  zelig2.lines <- .substitute.expressions(zelig2, model=model)
  param.lines <- .substitute.expressions(param, model=model)
  qi.lines <- .substitute.expressions(qi, model=model)
  describe.lines <- .substitute.expressions(describe, model=model)

  # write to file
  writeLines(zelig2.lines, con = zelig2.dest)
  writeLines(param.lines, con = param.dest)
  writeLines(qi.lines, con = qi.dest)
  writeLines(describe.lines, con = describe.dest)

  TRUE
}


#' make a description file for a specific package
#' param pkg a character-string specifying the name of the package
#' param author a vector of strings specifying the names of contributors
#' param email a character-string specifying the email of the maintainer
#' param depends a vector of strings specifying package dependencies
#' param url - ignored -
#' param path a character-string specifying the location of the package
#' return nothing
.make.description <- function (pkg, author, email, depends, url, path='.') {
  model <- pkg
  description.file <- file.path(path, model, 'DESCRIPTION')

  # make author list human-readable
  author <- .get.list.as.text(author)

  maintainer <- paste(author[1L], ' <', email, '>', sep="")

  depends <- c("Zelig", depends)
  depends <- unique(depends)
  depends <- paste(depends, collapse=", ")

  fields <- c(
      Package = model,
      Version = .1,
      Date = as.character(Sys.Date()),
      Title = "A Zelig Model",
      Author = author,
      Maintainer = maintainer,
      Depends = depends,
      Description = "A Zelig Model",
      License = "GPL (>=2)",
      URL = "http://gking.harvard.edu/zelig",
      Packaged = gsub('\\s+', ' ', date())
      )

  # correctly write to file:
  #   Package: 'model'
  #   Version: .1
  # etc.
  writeLines(
      paste(names(fields), ': ', fields, sep=""),
      con = description.file
      )
}


#' @note This function fails if passed non-alphanumeric variable names. In
#'   particular, the parameters cannot contain periods, etc.
#' @param .file the name of the file to replace
#' @param ... 
#' @return a character-string
.substitute.expressions <- function(.file, ...) {
  lines <- readLines(con = .file, warn = FALSE)

  replacements <- list(...)

  for (key in names(replacements)) {
    val <- replacements[[key]]
    expr <- paste('\\\\\\\\', key, '\\\\\\\\', sep="")

    lines <- gsub(expr, val, lines)
  }

  lines
}

#' Make \code{pkg}-package.R File for Roxygen Compliancy
#' @param pkg the package name
#' @param author a vector of characters specifying the authors of the Zelig
#'   models
#' @param email the email address of the package's maintainer
#' @param depends a vector specifying package dependencies
#' @param URL specifying the package's website
#' @param path location of the package
#' @return NULL
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.make.package.R <- function (pkg, author, email, depends, url, path='.') {
  file <- system.file('templates', 'PACKAGE.R', package='Zelig')
  dest <- file.path(path, pkg, 'R', paste(pkg, 'package.R', sep='-'))

  author <- .get.list.as.text(author)
  depends <- paste(c('Zelig', depends), collapse=', ', sep=', ')

  lines <- .substitute.expressions(author=author, package=pkg, .file=file,
    depends=depends
    )

  writeLines(lines, con = dest)
}


#' Convert Character-Strings into Human-Readable Lists
#' This functions converts its parameters into a human-readable and
#' grammatically correct series.
#' @param ... character-vectors and list of characters
#' @param final.comma whether to add the final comma to the series. Grammatical
#'   correctness is debateable
#' @return a comma delineated string
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.get.list.as.text <- function (..., final.comma=FALSE) {

  authors <- c(...)
  length <- length(authors)

  if (!length)
    ""

  else if (length == 1)
    authors[[1L]]

  else if (length == 2)
    paste(authors, collapse = " and ")

  else {
    beginning <- head(authors, -1)
    beginning <- paste(beginning, collapse= ', ')

    end <- tail(authors, 1)

    final.sep <- ifelse(final.comma, ', and ', ' and ')

    paste(beginning, end, sep = final.sep)
  }
}
#' Compute the Statistical Mode of a Vector
#' @param x a vector of numeric, factor, or ordered values
#' @return the statistical mode of the vector. If two modes exist, one is
#'   randomly selected (by design)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Mode <- function (x) {
  # build a table of values of x
  tab <- table(as.factor(x))

  # find the mode, then if there's more than one, select one randomly
  v <- sample(names(which(tab == max(tab))), size=1)

  # if it came in as a factor, we need to re-cast it
  # as a factor, with the same exact levels
  if (is.factor(x))
    return(factor(v, levels=levels(x)))

  # re-cast as any other data-type
  as(v, class(x))
}


#' Compute the Statistical Median of a Vector
#' @param x a vector of numeric or ordered values
#' @param na.rm ignored
#' @return the median of the vector
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Median <- function (x, na.rm=NULL) {
  v <- ifelse(is.numeric(x),
              median(v),
              levels(x)[ceiling(median(as.numeric(x)))]
              )
  if (is.ordered(x))
    v <- factor(v, levels(x))
  v
}

#' Compute the Maximum Value of a Vector
#' @param x a numeric or ordered vector
#' @param na.rm ignored
#' @return the maximum value of the vector
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Max <- function (x, na.rm=NULL) {
  if (is.numeric(x))
    return(max(x))
  
  else if (is.ordered(x))
    return(factor(max(levels(x),
                      na.rm=T
                      ),
                  levels=levels(x)
                  )
           )

  else
    stop("Error: max cannot be computed for non-numeric and non-ordered values")
}

#' Compute the Minumum Value of a Vector
#' @param x a vector of numeric or ordered values
#' @param na.rm ignored
#' @return the minimum value of the vector
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Min <- function (x, na.rm=NULL) {
  if (is.numeric(x))
    return(min(x))
  
  else if (is.ordered(x))
    return(factor(min(levels(x),
                      na.rm=T
                      ),
                  levels=levels(x)
                  )
           )

  else
    stop("Error: min cannot be computed for non-numeric and non-ordered values")
}
#' @export
loadDependencies <- function (..., character.only = FALSE) {
  # Get arguments that aren't "character.only"

  if (character.only) {
    packs <- match.call(expand.dots = TRUE)[-1]
    packs$character.only <- NULL
    packs <- as.character(packs)
  }
  else
    packs <- as.character(list(...))

  #
  results <- list()

  #
  for (pkg in packs)
    results[pkg] <- require(pkg, character.only = TRUE)

  if (all(unlist(results)))
    invisible(TRUE)
  else {
    failed.packs <- Filter(function (x) { return(x == FALSE) }, results)
    list.of.packages <- paste('"', names(failed.packs), '"', sep = '', collapse = ', ')

    message('The following packages did not load: ')
    cat('  ')
    message(list.of.packages)
    message()

    install.string <- paste('  install.packages(', names(failed.packs), ')', sep = '', collapse = '\n')

    message('To run this model, install these packages with the following command:')
    message(install.string)
    message()

    stop('')
  }
}

#' Produce All Combinations of a Set of Lists
#' @note This function is used internall by the 'mi' constructors in order to
#' produce the complete set of combinations of data-frames and factors by to
#' subset the data-frames.
#' @param ... a set of lists to mix together
#' @return all the combinations of the lists with repetition
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
mix <- function(...) {
  # expand dot arguments
  dots <- list(...)

  # error-catching
  if (length(dots) < 1)
    return(NULL)

  # prepare lists for first iteration
  res <- dots[[1]]
  dots <- dots[-1]

  # this entire algorithm could be optimized,
  # however, it will always be exponential time
  while(length(dots) > 0) {
    # get list to store new combinations in
    new.list <- list()

    # divide list
    first <- dots[[1]]

    # add new combinations
    for (f in first) {
      for (r in res) {
        row <- append(as.list(r), f)
        new.list[['']] <- row
      }
    }

    # Update list
    res <- new.list

    # Shift first entry off
    dots <- dots[-1]
  }

  # Appropriately name each entry
  for (k in 1:length(res))
    names(res[[k]]) <- names(list(...))

  res
}
#' Produce All Combinations of a Set of Lists
#' @note This function is used internall by the 'mi' constructors in order to
#'   produce the complete set of combinations of data-frames and factors by
#'   to subset the data-frames.
#' @param ... a set of lists to mix together
#' @return all the combinations of the lists with repetition
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
combine <- function(...) {
  # expand dot arguments
  dots <- list(...)

  # error-catching
  if (length(dots) < 1)
    return(NULL)

  # prepare lists for first iteration
  res <- dots[[1]]
  dots <- dots[-1]

  # this entire algorithm could be optimized,
  # however, it will always be exponential time
  while(length(dots) > 0) {
    # get list to store new combinations in
    new.list <- list()

    # divide list
    first <- dots[[1]]

    # add new combinations
    for (f in first)
      for (r in res)
        new.list[['']] <- c(r, f)

    # update list
    res <- new.list

    # shift first entry off
    dots <- dots[-1]
  }

  # m, as in matrix
  m <- NULL

  # format results as a matrix
  for (r in res)
    m <- rbind(m, r)

  # name rows/cols
  rownames(m) <- 1:length(res)
  colnames(m) <- names(list(...))

  # return
  m
}

#' Split a List into Two Lists
#' This functions takes any list, and splits into two lists - one containing
#' the values of arguments with specifically specified values and those without
#' specified values.
#' @note This function is a good candidate for deprecation
#' @param args a list
#' @return a list containing two entries: the key-value paired entires (titled
#'   wordful) and the unkeyed entried (titled wordless)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
#' @examples
#' #list(wordful = list(x=1, y=2), wordless=list(2, "red"))
splitUp <- function(args) {
  wordless <- list()
  wordful <- list()

  k <- 1

  if (is.null(names(args)))
    return(list(wordless=unlist(args), wordfull=NULL))

  for (key in names(args)) {
    if (nchar(key) == 0)
      wordless <- c(wordless, args[[k]])
    else
      wordful[[key]] <- args[[k]]

    k <- k+1
  }

  list(wordless=wordless, wordful=wordful)
}




# @topic: character-string representing help-topic
# @package: package containing help-topic
# return: character-string of processed Rd file
.get.help.file <- function(topic, package) {
  # get package help-file if no topic is set
  if (missing(topic))
    topic <- package
  
  # error-checking:
  #   ensure file and package are strings
  if (!is.character(topic) && length(topic) > 1L)
    stop()

  if (!is.character(package) && length(package) > 1L)
    stop()

  # 
  directory <- system.file(package=package)

  # 
  path <- utils:::index.search(
                               topic=topic,
                               paths=directory
                               )

  # search package-help-dataabase, get Rd file as string
  utils:::.getHelpFile(file=path)
}



# @package: character-string specifying the name of a package to
#           scan for help files
# @as.table: boolean specifying whether the return value will be
#            a table or names of Rd files
# return: either a named vector (table), or an unnamed vector
.list.help.files <- function(package, as.table=TRUE) {
  # index for help files
  fi <- file.path(
                  system.file(package=package),
                  "help",
                  "AnIndex"
                  )

  if (file.exists(fi)) {
    # get index of search-values and corresponding
    #  Rd file
    index <- scan(fi,
                  what = list(names="", values=""),
                  sep = "\t",
                  quote = "",
                  na.strings = "",
                  quiet = TRUE
                  )

    # the if-else below is a return value
    if (as.table)
      # return as an index
      structure(index$values, names=index$names)
    
    else
      # return only the names of the Rd files
      index$names
  }
  else {
    warning("nothing was found")
    NULL
  }
}

#' Compute the Intersection of Two Sets
#' @note This function is used internally by Zelig
#' @param a a vector
#' @param b a vector
#' @param unique a boolean determining whether a intersect b will contain only
#'   unique elements
#' @return the intersection of a and b
.intersection <- function(a, b, unique=TRUE) {
  intersection <- a[a %in% b]

  if (unique)
    intersection <- unique(intersection)

  if (is.null(intersection))
    c()
  else
    intersection
}

#' Hook to Update the Zelig Call with the Appropriate Call Object
#' @note This function is used internally by Zelig, and currently deprecated.
#' @param zobj a 'zelig' object
#' @param call1 the original call to Zelig
#' @param call2 the manuafactured call to the model fitting function
#' @return the 'zelig' object with a modified 'call' slot
replace.call <- function(zobj, call1, call2) {
  # what if it doesn't exist?
  if (!is.null(zobj$result$call) && is.call(zobj$result$call2))
    zobj$result$call <- call2

  zobj
}

#' Wether an Installed R-Pack Depends on Zelig
#' @note This package was used internally to determine whether an R-package is
#'   Zelig compliant, but is now likely deprecated. This test is useless if not
#'   paired with 
#' @param package a character-string naming a package
#' @return whether this package depends on Zelig
is.zelig.package <- function(package="") {
  "Zelig" %in% tools:::pkgDepends(package)$Depends
}

#' Whether a R-Package Contains a 'Yes' in its DESCRIPTION File's 'Zelig' Field
#' @note This package was used internally to determine whether an R-package is
#'   Zelig compliant, but is now likely deprecated.
#' @param package a character-string specifying an installed R-package
#' @return whether the package's DESCRIPTION file specifies Zelig-compliancy
#' @seealso is.zelig.package
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
is.zelig.compliant <- function(package="") {
  #
  zcomp <- packageDescription(package, fields="Zelig-Compliant")
  zcomp <- tolower(zcomp)

  #

  if (! zcomp %in% c('yes', 'no'))
    stop("")

  zcomp == "yes"
}
