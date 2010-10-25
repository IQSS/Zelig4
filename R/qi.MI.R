qi.MI <- function(z, x=NULL, x1=NULL, num=1000, param=NULL) {
  # split number of simulations between each model
  # this might not be intuitive for the user
  num <- round(num/length(z$result))


  # init list for qi's
  qi.list <- list()

  # initialize iterators
  zelig.iter <- iter(z$result)

  # define the null function
  null.function <- function(x) NULL

  # init setx with correct value
  if (is.null(x))
    setx.iter <- iter(null.function)
  else
    setx.iter <- iter(x$s.x)


 param.iter <- if (is.null(param))
   iter(null.function)
  else
    iter(param$results)

  # init setx1 with correct value
  if (is.null(x1))
    setx1.iter <- iter(null.function)
  else
    setx1.iter <- iter(x1$s.x)

  #
  repeat {
    # get items from iterators
    zelig.item <- try(nextElem(zelig.iter), silent=T)
    setx.item <- try(nextElem(setx.iter), silent=T)
    setx1.item <- try(nextElem(setx1.iter), silent=T)
    param.item <- try(nextElem(param.iter), silent=T)

    data <- NULL
    data.key <- try(nextElem(z$mi, keys.only=T), silent=T)

    names.data.key <- colnames(data.key)[-1]
    number <- data.key[[1]]
    data.key <- data.key[-1]

    # if end of list
    if (inherits(zelig.item, "try-error") ||
        inherits(setx.item, "try-error") ||
        inherits(setx1.item, "try-error") ||
        inherits(data, "try-error") ||
        inherits(param.item, "try-error"))
      break

    # create a kin object
    kin <- zelig.kin(z, zelig.item, data=data)

    label <- paste(names.data.key, data.key, sep=" = ", collapse=", ")

    if (nchar(label) < 1 || is.null(label))
      label <- paste("data-set", number)

    else
      label <- paste(paste("data-set", number), label, sep=" | ")

    # compute qi
    res.qi <- qi(kin, x=setx.item, x1=setx1.item, num=num, param=param.item)
    qi.list[[label]] <- as.qi( res.qi )
  }

  # build object
  res <- list(results = qi.list,
              names   = names(res.qi),
              by      = z$by,
              levels  = names(qi.list)
              )

  class(res) <- c("MI", "qi")
  res
}
