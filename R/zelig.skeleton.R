#' creates a skeleton zelig package
zelig.skeleton <- function (
    pkg, models=c(), author=c(),
    environment = .GlobalEnv,
    path = ".",
    force = FALSE,
    email = "",
    depends = c()
    ) {

  e <- new.env()

  for (m in models) {
    # place proper functions in
    # correct environment (out of global)
    .generate.functions(m, e)
  }

  package.skeleton(
      name = pkg,
      environment = e,
      path = path,
      force = force,
      namespace = TRUE
      )

  for (m in models) {
    .copy.templates(m, pkg, path)
  }

  print(ls(envir=e))


  .make.description(pkg, author, email, depends, url, path)
}


#' creates _necessary_ files for a model
.copy.templates <- function (model, pkg, path) {
  zelig2 <- system.file('templates', 'zelig2.R', package="Zelig")
  param <- system.file('templates', 'param.R', package="Zelig")
  qi <- system.file('templates', 'qi.R', package="Zelig")

  zelig2.dest <- file.path(path, pkg, 'R', paste('zelig2', model, '.R', sep=""))
  param.dest <- file.path(path, pkg, 'R', paste('param', model, 'R', sep="."))
  qi.dest <- file.path(path, pkg, 'R', paste('qi', model, 'R', sep="."))

  #file.copy(zelig2, zelig2.dest)
  #file.copy(param, param.dest)
  #file.copy(qi, qi.dest)

  # create blank files
  file.create(zelig2.dest, param.dest, qi.dest)
}


#'
#
.make.description <- function (model, author, email, depends, url, path='.') {
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

  fields <- c(
      Package = model,
      Version = .1,
      Date = as.character(Sys.Date()),
      Title = "A Zelig Model",
      Author = author,
      Maintainer = email,
      Depends = paste("Zelig", depends, collapse=', ', sep=", "),
      Description = "A Zelig Model",
      License = "GPL (>=2)",
      URL = "http://gking.harvard.edu/zelig",
      Packaged = gsub('\\s+', ' ', date())
      )

  # ...
  writeLines(
      paste(names(fields), ': ', fields, sep=""),
      con = description.file
      )
}

.authors <- function(authors) {
  if (length(authors) > 1)
    paste(
      paste(head(authors, -1), collapse=", "),
      'and',
      tail(authors, 1)
      )

  else
    authors
}


#' generate zelig2, qi., and param. functions for model
#' in a particular environment
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
