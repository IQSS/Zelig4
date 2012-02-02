\name{as.qi}
\alias{as.qi}
\title{Generic Method for Converting Various Objects into 'qi' Objects
'qi' objects are list-style objects used by the 'summarize' function to
compute simple summaries about the simulated data. For readability and
and simplicity purposes, the 'qi' function typically returns a list of
named simulations. This list is converted internally by Zelig into a 'qi'
object so that several methods can be easily applied to the Quantities of
Interest: plot, summarize, and print}
\usage{
  as.qi(s)
}
\arguments{
  \item{s}{the object to be casted}
}
\value{
  an object of type `qi'
}
\description{
  Generic Method for Converting Various Objects into 'qi'
  Objects 'qi' objects are list-style objects used by the
  'summarize' function to compute simple summaries about
  the simulated data. For readability and and simplicity
  purposes, the 'qi' function typically returns a list of
  named simulations. This list is converted internally by
  Zelig into a 'qi' object so that several methods can be
  easily applied to the Quantities of Interest: plot,
  summarize, and print
}
\note{
  These functions are primarily used internall by Zelig and
  should not be used in the Global namespace.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}
\seealso{
  as.qi.default as.qi.qi as.qi.list
}

