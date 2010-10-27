run <- function(zc, ..., data=NULL) UseMethod("run")

#
run.ZeligCall <- function(zc, ..., data=NULL) {
  # attach data.frame's for compatibility issues
  if (is.data.frame(data))
    attach(data)


  # maybe replace data.frame?
  if (!is.null(data)) {
    zc$parameters$data <- NULL
    zc$paremeters <- append(zc$parameters, alist(data=data))
  }

  # run function
  res <- do.call(zc$model, zc$parameters)

  # detach
  if (is.data.frame(data))
    detach(data)

  res
}
