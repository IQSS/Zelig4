#' Search for, Copy, and Customize Template for a Newly Create Zelig Package
#' This is used internally by \code{zelig.skeleton}
#' @param model a character-string specifying the name of the model
#' @param the file template to search for and copy
#' @param pkg a character-string specifying the name of the package
#' @param path a character-string specifying the base path to the package's u
#'  parent directory
#' @return This function is used for its side-effects.
.copy.templates <- function (model, pkg, path) {
  # path to R folder
  r.path <- file.path(path, pkg, 'R')

  # source files
  zelig2 <- system.file('templates', 'zelig2.R', package="Zelig")
  param <- system.file('templates', 'param.R', package="Zelig")
  qi <- system.file('templates', 'qi.R', package="Zelig")
  describe <- system.file('templates', 'describe.R', package="Zelig")

  # create R directory
  dir.create(r.path, showWarnings=FALSE)

  # destination files
  zelig2.dest <- file.path(r.path, paste('zelig2', model, '.R', sep=""))
  param.dest <- file.path(r.path, paste('param', model, 'R', sep="."))
  qi.dest <- file.path(r.path, paste('qi', model, 'R', sep="."))
  describe.dest <- file.path(r.path, paste('describe', model, 'R', sep="."))

  # create blank files
  file.create(zelig2.dest, param.dest, qi.dest)

  # substitute
  zelig2.lines <- .substitute.expressions(zelig2, model=model)
  param.lines <- .substitute.expressions(param, model=model)
  qi.lines <- .substitute.expressions(qi, model=model)
  describe.lines <- .substitute.expressions(describe, model=model)

  # write to file
  writeLines(zelig2.lines, con = zelig2.dest)
  writeLines(param.lines, con = param.dest)
  writeLines(qi.lines, con = qi.dest)
  writeLines(describe.lines, con = describe.dest)

  TRUE
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
  author <- .get.list.as.text(author)

  maintainer <- paste(author[1L], ' <', email, '>', sep="")

  depends <- c("Zelig", depends)
  depends <- unique(depends)
  depends <- paste(depends, collapse=", ")

  fields <- c(
      Package = model,
      Version = .1,
      Date = as.character(Sys.Date()),
      Title = "A Zelig Model",
      Author = author,
      Maintainer = maintainer,
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


#' @note This function fails if passed non-alphanumeric variable names. In
#'   particular, the parameters cannot contain periods, etc.
#' @param .file the name of the file to replace
#' @param ... 
#' @return a character-string
.substitute.expressions <- function(.file, ...) {
  lines <- readLines(con = .file, warn = FALSE)

  replacements <- list(...)

  for (key in names(replacements)) {
    val <- replacements[[key]]
    expr <- paste('\\\\\\\\', key, '\\\\\\\\', sep="")

    lines <- gsub(expr, val, lines)
  }

  lines
}

#' Make \code{pkg}-package.R File for Roxygen Compliancy
#' @param pkg the package name
#' @param author a vector of characters specifying the authors of the Zelig
#'   models
#' @param email the email address of the package's maintainer
#' @param depends a vector specifying package dependencies
#' @param URL specifying the package's website
#' @param path location of the package
#' @return NULL
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.make.package.R <- function (pkg, author, email, depends, url, path='.') {
  file <- system.file('templates', 'PACKAGE.R', package='Zelig')
  dest <- file.path(path, pkg, 'R', paste(pkg, 'package.R', sep='-'))

  author <- .get.list.as.text(author)
  depends <- paste(c('Zelig', depends), collapse=', ', sep=', ')

  lines <- .substitute.expressions(author=author, package=pkg, .file=file,
    depends=depends
    )

  writeLines(lines, con = dest)
}


#' Convert Character-Strings into Human-Readable Lists
#' This functions converts its parameters into a human-readable and
#' grammatically correct series.
#' @param ... character-vectors and list of characters
#' @param final.comma whether to add the final comma to the series. Grammatical
#'   correctness is debateable
#' @return a comma delineated string
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.get.list.as.text <- function (..., final.comma=FALSE) {

  authors <- c(...)
  length <- length(authors)

  if (!length)
    ""

  else if (length == 1)
    authors[[1L]]

  else if (length == 2)
    paste(authors, collapse = " and ")

  else {
    beginning <- head(authors, -1)
    beginning <- paste(beginning, collapse= ', ')

    end <- tail(authors, 1)

    final.sep <- ifelse(final.comma, ', and ', ' and ')

    paste(beginning, end, sep = final.sep)
  }
}
