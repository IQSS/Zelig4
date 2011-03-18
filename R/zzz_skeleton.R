#' creates _necessary_ files for a model
#' param model a character-string specifying the name of the model
#' param pkg a character-string specifying the name of the package
#' param path a character-string specifying the base path to the package
#' return nothing
.copy.templates <- function (model, pkg, path) {
  # path to R folder
  r.path <- file.path(path, pkg, 'R')

  # source files
  #zelig2 <- system.file('templates', 'zelig2.R', package="Zelig")
  #param <- system.file('templates', 'param.R', package="Zelig")
  #qi <- system.file('templates', 'qi.R', package="Zelig")

  # create R directory
  dir.create(r.path, showWarnings=FALSE)

  # destination files
  zelig2.dest <- file.path(r.path, paste('zelig2', model, '.R', sep=""))
  param.dest <- file.path(r.path, paste('param', model, 'R', sep="."))
  qi.dest <- file.path(r.path, paste('qi', model, 'R', sep="."))

  # create blank files
  file.create(zelig2.dest, param.dest, qi.dest)
}


#' make a description file for a specific package
#' param pkg a character-string specifying the name of the package
#' param author a vector of strings specifying the names of contributors
#' param email a character-string specifying the email of the maintainer
#' param depends a vector of strings specifying package dependencies
#' param url - ignored -
#' param path a character-string specifying the location of the package
#' return nothing
.make.description <- function (pkg, author, email, depends, url, path='.') {
  model <- pkg
  description.file <- file.path(path, model, 'DESCRIPTION')

  # make author list human-readable
  author <- if (length(author) > 1)
    paste(
      paste(head(author, -1), collapse=", "),
      'and',
      tail(author, 1)
      )

  else
    author


  depends <- c("Zelig", depends)
  depends <- unique(depends)
  depends <- paste(depends, collapse=", ")

  fields <- c(
      Package = model,
      Version = .1,
      Date = as.character(Sys.Date()),
      Title = "A Zelig Model",
      Author = author,
      Maintainer = email,
      Depends = depends,
      Description = "A Zelig Model",
      License = "GPL (>=2)",
      URL = "http://gking.harvard.edu/zelig",
      Packaged = gsub('\\s+', ' ', date())
      )

  # correctly write to file:
  #   Package: 'model'
  #   Version: .1
  # etc.
  writeLines(
      paste(names(fields), ': ', fields, sep=""),
      con = description.file
      )
}


#' generate zelig2, qi., and param. functions for model
#' in a particular environment
#' param model a character-string specifying the name of a non-existent Zelig model
#' param env an environment 
#' return invisible environment variable (with stored functions)
.generate.functions <- function(model, env) {
  # define zelig2 function
  zelig2 <- function (formula, ..., data) {
    list(
        .function = ""
        )
  }

  # define param function
  param <- function (obj, num) {
    list(
        coef  = NULL
        )
  }


  # define qi function
  qi <- function (obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {
    list(
        "Expected Value: E(Y|X)" = NA
        )
  }

  # ...
  zelig2model <- paste('zelig2', model, sep="")
  param.model <- paste('param', model, sep=".")
  qi.model <- paste('qi', model, sep=".")


  assign(zelig2model, zelig2, envir=env)
  assign(param.model, param, envir=env)
  assign(qi.model, qi, envir=env)

  invisible(env)
}
