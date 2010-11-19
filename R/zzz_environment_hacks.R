# @params: a list of parameters to create a Zelig Call
# return: a list containing:
#           * parameters - a list of symbols
#           * envir - an environment where variables
#             contained in envir exist
.store.values <- function(params) {
  #
  e <- new.env()

  #
  for (key in names(params))
    assign(key, params[[key]], envir=e)

  #
  list(
       params=Map(as.name, names(params)),
       envir = e
       )
}
