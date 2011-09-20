#' 'zelig.skeleton' generates the necessary files used to create a Zelig
#' package. Based on (and using) R's 'package.skeleton' it removes some of the
#' monotony of building statistical packages. In particular, 'zelig.skeleton'
#' produces templates for the \code{zelig2}, \code{describe}, \code{param}, and
#' \code{qi} methods. For more information about creating these files on an
#' individual basis, please refer to the tech manuals, which are available 
#' by typing: \code{?zelig2}, \code{?param}, or \code{?qi}.
#' @title Creates a Skeleton for a New Zelig package
#' @param pkg a character-string specifying the name of the Zelig package
#' @param models a vector of strings specifying models to be included in the
#'   package
#' @param author a vector of strings specifying contributors to the package
#' @param path a character-string specifying the path to the package
#' @param force a logical specifying whether to overwrite files and create
#'   necessary directories
#' @param email a string specifying the email address of the package's
#'   maintainer
#' @param depends a vector of strings specifying package dependencies
#' @param ... ignored parameters
#' @param .gitignore a logical specifying whether to include a copy of a 
#'   simple \code{.gitignore} in the appropriate folders (\code{inst/doc} and
#'   the package root
#' @param .Rbuildignore a logical specifying whether to include a copy of a 
#'   simple \code{.Rbuildignore} in the appropriate folders (\code{inst/doc} 
#'   and the package root
#' @return nothing
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig.skeleton <- function (
    pkg, models=c(), author="UNKNOWN AUTHOR",
    path = ".",
    force = FALSE,
    email = "maintainer@software-project.org",
    depends = c(),
    ...,
    .gitignore = TRUE,
    .Rbuildignore = TRUE
    ) {


  # WARNING BLOCK
  # so that developers are aware of potential pitfalls that will prevent
  # installation of their packages
  if (!is.character(pkg)) {
    warning("invalid 'pkg' parameter; should be a character string")
    pkg <- as.character(pkg)
  }

  if (length(pkg) > 1) {
    warning("invalid 'pkg' parameter; length cannot be greater than one")
    pkg <- pkg[1]
  }

  if (!is.character(models)) {
    warning("invalid 'models' parameter; should be a character vector")
    models <- as.character(models)
  }

  if (!length(models))
    warning("invalid 'models' parameter; should contain at least one model-name")

  if (missing(author))
    warning("missing 'author' parameter; please change the value in the",
      "'DESCRIPTION' file's 'Author' field")

  if (missing(email))
    warning("Missing 'email' parameter; please change the value in the ",
      "'DESCRIPTION' file's 'Maintainer' field")

  if (missing(depends))
    warning("Missing 'depends' parameter")

  # new environment
  e <- new.env()

  for (m in models) {
    # Place proper functions in
    # correct environment (out of global)
    # this technically doesn't work
    # (bug in package.skeleton)
    describe <- function (...) list()
    zelig2 <- function (formula, ..., data) list(.function = "")
    param <- function (obj, num, ...) list(coef=NULL)
    qi <- function (obj, x, x1, y, param, num) list()

    assign(paste("describe", m, sep="."), describe, e)
    assign(paste("zelig2", m, sep=""), describe, e)
    assign(paste("param", m, sep="."), describe, e)
    assign(paste("qi", m, sep="."), describe, e)
  }

  # Invoke package.skeleton
  package.skeleton(
                   name = pkg,
                   environment = e,
                   path = path,
                   force = force,
                   namespace = TRUE
                   )

  # Copy files over - as of 3/11 these files are blank
  for (m in models) {
    .copy.templates(m, pkg, path)
  }

  .make.description(pkg, author, email, depends, url, path)
  .make.package.R(pkg, author, email, depends, url, path)

  # copy .gitignore and .Rbuildignore
  if (.gitignore) {
    src <- system.file('hidden', 'gitignore', package='Zelig')
    dest <- file.path(path, pkg, '.gitignore')
    file.copy(src, dest)

    dest <- file.path(path, pkg, 'man', '.gitignore')
    file.copy(src, dest)
  }

  if (.Rbuildignore) {
    src <- system.file('hidden', 'Rbuildignore', package='Zelig')
    
    dest <- file.path(path, pkg, '.Rbuildignore')
    file.copy(src, dest)

    dest <- file.path(path, pkg, 'inst', 'doc', '.Rbuildignore')
    dir.create(file.path(path, pkg, 'inst', 'doc'), recursive=TRUE)
    file.copy(src, dest)
  }


  # Why zero? Eh, maybe a return code thing. This function is really just used
  # for side-effects
  invisible(0)
}
