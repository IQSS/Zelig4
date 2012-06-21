#' Generic Method for ``bootstrap''
#'
#' This method is intended to be overried by statistical models that would like
#' to support statistical bootstrapping.
#' @note This method has private memory storage and can reference the objects:
#' ``.fitted'', ``.data'', ``.call'', ``.env'', despite having no declaration in
#' the argument list.
#' @param obj a fitted model object that will be used to produce boot-strapped
#' parameters. This object usually inherits the class ``glm'' or ``lm'' object
#' @param ... unspecified parameters
#' @return a list with the ``alpha'' and ``beta'' slots set. Note that ``alpha''
#' corresponds to ancillary parameters and ``beta'' corresponds to systematic
#' components of the model
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
bootstrap <- function (obj, ...)
  UseMethod("bootstrap")

#' Produce Boot-strapped Parameters for a Statistical Model
#'
#' This method is a fallback for bootstrapping models that do not have a defined
#' ``bootstrap'' method. For most models, this default is sufficient, so long as
#' the model follows the usual convention that ``coef(obj)'' returns the
#' systematic parameters of a fitted model.
#' @usage \method{bootstrap}{default}(obj, ...)
#' @S3method bootstrap default
#' @param obj a fitted model object. This is typically of type ``glm'' or ``lm''
#' @param ... unspecified parameters
#' @return a list with the ``alpha'' and ``beta'' slots set
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
bootstrap.default <- function (obj, ...)
  list(
       alpha = NULL,
       beta = coef(obj)
       )
