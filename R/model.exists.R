#' Whether a Particular Model Can Be Run
#' 
#' Returns whether a particular model can be read.
#' @param model
#' @param verbose a logical specifying whether 
model.exists <- function (model, verbose = TRUE, ...) {

  if (!is.character(model))
    return(FALSE)

  pkg <- get.package(model)

  if (is.na(pkg)) {
    message("\n\n")
    cat(sprintf('The model %s is not available in this package.', model))
    message("\n\n")

    FALSE
  }

  else if (! pkg %in% loadedNamespaces()) {
    if (pkg %in% .packages(TRUE)) {
      message("\n\n")
      cat(sprintf('The model %s belongs to the package %s!\n\n', model, pkg))
      cat(sprintf('In order to use this model, first load %s', pkg))
      cat(' with the following command:\n')
      message(sprintf('  library(%s)', pkg))
      message("\n\n")
    }

    else {
      message("\n\n")
      cat(sprintf('The model %s belongs to the package %s!\n\n', model, pkg))
      cat(sprintf('In order to use this model, first install %s', pkg))
      cat(' with the following command:\n')

      message(sprintf(
                      '  install.packages("%s", repos=%s, type="source")', 
                      pkg, "http://r.iq.harvard.edu/"
                      ))

      message("\n\n")
    }

    FALSE
  }

  else
    TRUE
}
