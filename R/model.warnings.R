# This code is rough looking. It needs to be made more elegant
# but R doesn't really support block quotes
model.warnings <- function (model) {
  # Get appropriate Zelig package
  pkg <- get.package(model)

  # Get 
  zelig2 <- paste("zelig2", as.character(model), sep="")
  zelig2 <- tryCatch(
                     { get(zelig2, mode="function"); 1 },
                     error = function (e) NA
                     )

  #
  #
  #
  if (is.na(zelig2) && is.na(pkg)) {

      msg <- '

** The model "%s" is not available with the currently loaded packages,
** and is not an official Zelig package.
** The model\'s name may be a typo.

'
    message(sprintf(msg, model))

  }


  else if (is.na(zelig2) && !is.na(pkg)) {

    if (pkg %in% .packages(TRUE)) {
      # The package is available on the system

      msg <- '

** The model "%s" is not available with the currently loaded packages,
** however it *is* installed on your system.
**
** To load this model\'s package, please type:
library("%s")

'
      message(sprintf(msg, model, pkg))
    }

    #
    #
    #
    else {
      # Else... the package is not available on the system

      repos <- "http://r.iq.harvard.edu/"
      msg <- '

** The model "%s" is not installed on your system,
** however it *is* available for download from Harvard.
**
** To install and load this model\'s package, please type:
install.packages("%s", repos="%s", type="source")
library("%s")

'
      message(sprintf(msg, model, pkg, repos, pkg))
    }
  }


  invisible()
}
