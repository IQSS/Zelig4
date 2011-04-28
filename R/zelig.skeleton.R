#' Creates a skeleton zelig package
#' @param pkg a character-string specifying the name of the Zelig package
#' @param models a vector of strings specifying models to be included in the package
#' @param author a vector of strings specifying contributors to the package
#' @param environment - ignored -
#' @param path a character-string specifying the path to the package
#' @param force a logical specifying whether to overwrite files and create necessary directories
#' @param email a string specifying the email address of the package's maintainer
#' @param depends a vector of strings specifying package dependencies
#' @return nothing
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig.skeleton <- function (
    pkg, models=c(), author=c(),
    environment = .GlobalEnv,
    path = ".",
    force = FALSE,
    email = "",
    depends = c()
    ) {

  # new environment
  e <- new.env()

  for (m in models) {
    # place proper functions in
    # correct environment (out of global)
    # this technically doesn't work
    # (bug in package.skeleton)
    .generate.functions(m, e)
  }

  # invoke package.skeleton
  package.skeleton(
      name = pkg,
      environment = e,
      path = path,
      force = force,
      namespace = TRUE
      )

  # copy files over - as of 3/11 these files are blank
  for (m in models) {
    .copy.templates(m, pkg, path)
  }

  .make.description(pkg, author, email, depends, url, path)
}
