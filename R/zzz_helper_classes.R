# THIS FILE CONTAINS HELPER CLASSES FOR ZELIG
# -------------------------------------------

# @parent: the parent model (MI)
# @fitted: the fitted model object
# @data:   a new data-frame
# return:  a zelig object
zelig.kin <- function(parent, fitted, data=NULL) {
  # build a list in an obvious way
  z <- list(name = parent$name,
            formula = parent$formula,
            result = fitted,
            args = parent$args,
            data = data,
            call = parent$call,
            is.MI = F,
            by = NULL,
            mi = iter(list()),
            model.obj = parent$model.obj
            )

  # make zelig object, and return
  class(z) <- c(z$name, "zelig")
  z  
}
# dummy zelig class - we should actually make this a bit more rigorous
setClass("zelig", representation())
