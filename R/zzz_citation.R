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
.cite.zelig.tex <- function() {
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
