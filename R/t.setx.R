# @x: setx object
# return: a transposed matrix
# **note: function used to maintain both backwards
#         compatibility and ease-of-use
t.setx <- function(x)
  t(x$matrix)
