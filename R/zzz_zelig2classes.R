# @export literal
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
