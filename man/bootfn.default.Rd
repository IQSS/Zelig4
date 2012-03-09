\name{bootfn.default}
\alias{bootfn.default}
\title{Default Boot-strapping procedure}
\usage{
  bootfn.default(data, i, object)
}
\arguments{
  \item{data}{a data.frame}

  \item{i}{an integer or chacter-string specifying the
  index of the row to be used in the bootstrapping
  procedure.}

  \item{object}{the fitted model object}
}
\value{
  a list of paramters
}
\description{
  The default procedure for extracting bootstrap
  information. Note that this method re-fits the data and
  resamples the data frequently. This is a good candidate
  for fixing-up.
}

