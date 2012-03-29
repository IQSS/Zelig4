\name{name.object}
\alias{name.object}
\title{Name Elements of an Object}
\usage{
  name.object(obj, names)
}
\arguments{
  \item{object}{a vector or matrix}

  \item{names}{a character-vector specifying names}
}
\value{
  the original object, with a "colnames" or "names" equal
  to the parameter "names". If "names" is larger than
  "obj", the "names" parameter is truncated appropriately.
  If it is smaller, then the latter part of "obj" is
  replaced with a numbered generic column name
}
\description{
  xxx
}
\note{
  ...
}

