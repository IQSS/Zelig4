print.zelig <- function(z) {
  message()
  cat("name = ")
  message(z$name)

  cat("arguments = ")
  message(paste(z$args, collapse=", "))

  cat("data columns = ")
  message(paste(colnames(z$data), collapse=", "))

  message("result object")
  print(summary(z$result))
}
