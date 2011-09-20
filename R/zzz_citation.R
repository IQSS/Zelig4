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
