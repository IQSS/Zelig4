#' Prompts user to hit enter
#' @title Prompt User
#' @param msg a character-string, specifying a message to be displayed
#' @return This function is used for its side effects
#' @export
#' @note This function is primarily used by Zelig demo scripts
user.prompt <- function (msg = NULL) {
  if (is.null(msg))
    msg <- "Press <return> to continue: "

  msg <- paste("\n", msg, sep="")

  invisible(readline(msg))
}
