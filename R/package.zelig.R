package.zelig <- function(name, version="1.0") {

  if (!is.character(name) ||  length(name) > 1)
    stop()

  if (!(is.numeric(version) || is.character(version)) ||
      length(version) > 1)
    stop()

  self <- list(name = as.character(name),
               version = as.character(version)
               )
  self
}

as.list.package.zelig <- function(pz, ...)
  list(name=pz$name, version=pz$version)
