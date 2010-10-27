# @zelig.object: a zelig object
# return: a character vector containing names
#         of S3-methods for zelig$result
register.default <- function(zelig.object) {
  .GetGenerics(zelig.object)
}
