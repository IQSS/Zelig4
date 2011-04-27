#' Force Evaluation of Parameter to the Call of the Foreign Model
#' Zelig typically stores return-values of 'zelig2' functions in an insulated
#' environment. This prevents this value from being stored, so that it can be
#' evaluated before being stored away.
#' @note This function is used by developers writing 'zelig2' functions. This
#'   tool comes in particularly useful when using exotic 'formula' objects and
#'   model frames.
#' @param obj any type of object
#' @param type this parameter is currently ignored by Zelig
#' @return a 'literal' object, which will be evaluated before being stored in
#"   the 'zelig' object's stored environment
#' @export literal
literal <- function (obj, type = NULL) {

  call <- match.call()

  self <- list(
               value = obj,
               type = type,
               call = call
               )

  class(self) <- "literal"
  self
}
