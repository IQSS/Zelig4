# THIS FILE CONTAINS CITATION-RELATED FUNCTIONS
# (PREVIOUSLY vdc.R) --------------------------
# ---------------------------------------------

# @model.name: 
# @...: nothing (for backwards compatibility
.ZeligDescribeModel <- function(model.name, ...) {
  # lie to zelig
  dummy.zelig <- "dummy"
  class(dummy.zelig) <- model.name

  # return as a description
  as.description(describe(dummy.zelig))
}


# @zelig.only: a boolean specifying whether we want to search
#              only the Zelig namespace
# return: a list of the loaded zelig models
ZeligListModels <- function(zelig.only=FALSE) {
  results <- if (zelig.only)
    ls(pattern="^zelig2", envir=asNamespace("Zelig"))
  else
    apropos("^zelig2", mode="function")

  # substitute and return
  sub("^zelig2", "", results)
}

.GetModelCitationTex <- function(model.name)
  cite(ZeligDescribeModel(model.name))

ZeligDescribeModel <- function(model.name, force=FALSE, schema="1.1") {
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

# return: list of available model categories
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

.describe <- function(model.name) {
  cite(ZeligDescribeModel(model.name))
}


# output: zelig's full citation
.cite.zelig <- function() {
  message()
  message("To cite Zelig as a whole, please reference these two sources:")
  message(
"  Kosuke Imai, Gary King, and Olivia Lau. 2007. ``Zelig: Everyone's
  Statistical Software,'' http://gking.harvard.edu/zelig/.

  Kosuke Imai, Gary King, and Olivia Lau.  2008.  ``Toward a Commmon
  Framework for Statistical Analysis and Development.'' Journal of
  Computational and Graphical Statistics, Vol. 17, No. 4 (December),
  pp. 892-913.

")
}


# return: TeX style citation for producing Zelig documentation
#         
cite.zelig.tex <- .cite.zelig.tex <- function() {
  paste(
        "To cite Zelig as a whole, please reference these two sources:",
        "\\begin{verse}",
        "  Kosuke Imai, Gary King, and Olivia Lau. 2007. ``Zelig: Everyone's",
        "  Statistical Software,'' http://gking.harvard.edu/zelig/.",
        "\\end{verse}",
        "\\begin{verse}",
        "  Kosuke Imai, Gary King, and Olivia Lau.  2008.  ``Toward a Commmon",
        "  Framework for Statistical Analysis and Development.'' Journal of",
        "  Computational and Graphical Statistics, Vol. 17, No. 4 (December),",
        "  pp. 892-913.",
        "\\end{verse}",
        sep="\n"
        )
}


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











# @pkg: a character-string representing a package name
# return: whether the package contains any zelig2-functions
has.zelig2 <- function(pkg) {

  env <- asNamespace(pkg)

  hits <- grep("^zelig2*", ls(envir=env))

  length(hits) > 0
}


# @package: a character-string representing a package name
# return: whether the package lists Zelig as a dependency
#         in its DESCRIPTION file
depends.on.zelig <- function(package="") {
  #
  zcomp <- packageDescription(package, fields="Depends")

  if (is.na(zcomp))
    return(FALSE)

  zcomp <- unlist(strsplit(zcomp, " *, *"))


  "Zelig" %in% zcomp
}


# return: list of all zelig-dependent packages
list.zelig.dependent.packages <- function() 
  Filter(depends.on.zelig, .packages(all.available=TRUE))


# return: list of all zelig models
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
