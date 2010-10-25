\name{zelig}

\alias{zelig}

\title{Estimating a Statistical Model}

\description{ The \code{zelig} command estimates a variety of statistical
models.  Use \code{zelig} output with \code{setx} and \code{sim} to compute
quantities of interest, such as predicted probabilities, expected values, and
first differences, along with the associated measures of uncertainty
(standard errors and confidence intervals). }

\usage{
z.out <- zelig(formula, model, data, by, save.data, cite, \dots) 
}

\arguments{
\item{formula}{a symbolic representation of the model to be
  estimated, in the form \code{y \~\, x1 + x2}, where \code{y} is the
  dependent variable and \code{x1} and \code{x2} are the explanatory
  variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
  same dataset.  (You may include more than two explanatory variables,
  of course.)  The \code{+} symbol means ``inclusion'' not
  ``addition.''  You may also include interaction terms and main
  effects in the form \code{x1*x2} without computing them in prior
  steps; \code{I(x1*x2)} to include only the interaction term and
  exclude the main effects; and quadratic terms in the form
  \code{I(x1^2)}.  }
\item{model}{the name of a statistical model, enclosed in \code{""}.
  Type \code{help.zelig("models")} to see a list of currently supported
  models.  }
\item{data}{the name of a data frame containing the variables
  referenced in the formula, or a list of multiply imputed data frames
  each having the same variable names and row numbers (created by
  \code{mi}). }

\item{save.data}{If is set to "TRUE", the input dataframe will be saved
  as an attribute ("zelig.data") of the zelig output object. }

\item{cite}{If is set to "TRUE" (default), the model citation will be
  will be printed out when this function is called. }

\item{by}{a factor variable contained in \code{data}.  Zelig will subset
the data frame based on the levels in the \code{by} variable, and
estimate a model for each subset.  This a particularly powerful option
which will allow you to save a considerable amount of effort.  For
example, to run the same model on all fifty states, you could type:
\code{z.out <- zelig(y ~ x1 + x2, data = mydata, model = "ls", by = "state")}
  You may also use \code{by} to run models using MatchIt subclass.  }
\item{\dots}{additional arguments passed to \code{zelig},
  depending on the model to be estimated. }
}

\value{
Depending on the class of model selected, \code{zelig} will return
an object with elements including \code{coefficients}, \code{residuals},
and \code{formula} which may be summarized using
\code{summary(z.out)} or individually extracted using, for example,
\code{z.out\$coefficients}.  See the specific models listed above
for additional output values, or simply type \code{names(z.out)}.  
}

\seealso{ The full Zelig manual is available at
  \url{http://gking.harvard.edu/zelig}.
}

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>
}

\keyword{file}
























