\name{description}
\alias{description}
\title{Constructor for the 'description' class}
\usage{
  description(authors = c("Kosuke Imai", "Gary King", "Olivia Lau"),
    year = NULL, model = "", text = "", url = "",
    category = NULL)
}
\arguments{
  \item{authors}{a character-vector of author names}

  \item{year}{a numeric specifying the year}

  \item{model}{a character-string specifying model name}

  \item{text}{a character-string specifying the title of
  the model. This typically includes more exact information
  than 'model'. E.g., for the 'logit' the title 'Logistic
  Regression for Dichotomous Variables' would be a suitable
  text parameter.}

  \item{url}{a character-string specifying the model's
  software page}

  \item{category}{deprecated until data-verse bindings are
  reevaluated}
}
\value{
  an object of type 'description'
}
\description{
  Constructor for the 'description' class
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

