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
    ls(pattern="^zelig2\\.", envir=asNamespace("Zelig"))
  else
    apropos("^zelig2\\.", mode="function")

  # substitute and return
  sub("^zelig2\\.", "", results)
}

.GetModelCitationTex <- function(model.name) {
  # next line should actually be most excellent
  dummy <- "Be Excellent to Everyone."
  class(dummy) <- model.name

  # compute dummy object
  cite.obj <- describe(model.name)

  # return
  cite(cite.obj)
}

ZeligDescribeModel <- function(model.name, force=FALSE, schema="1.1") {
  dummy <-
    "I love baseball.  You know, it doesn't have to mean anything.
It's just very beautiful to watch."
  
  class(dummy) <- model.name

  # describe
  describe(dummy)
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

.describe <- function(model) {
  dummy <- "I love baseball.  You know, it doesn't have to mean anything."
  class(dummy) <- model
  describe(model)
}
