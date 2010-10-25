print.setx <- function(z) {
  # new-line
  message()

  # print model name
  cat("model name: ")
  message(format(z$name, width=12))

  # print formula
  cat(format("formula: ", width=12))
  print(z$formula)

  # print whether cond is True or False
  cat(format("cond: ", width=12))
  message(ifelse(z$cond, "Yes", "No"))

  # print the results of computations
  # run on specific variables (or data columns)
  message("results of fn:")

  # set a maximum width based on names of
  # predictor variables
  max.width <- max(nchar(names(z$values)))

  # print the key, value pairs with formatting
  for (key in names(z$values)) {
    text <- format(sprintf("  %s ", key),
                   width=max.width+3,
                   justify="left")
    cat(text)
    cat("= ")
    message(z$values[[key]])
  }

  # new-line
  message()

  # return invisibly
  invisible(z)
}
