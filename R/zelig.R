# author: Matt Owen
# date:   7/1/2010
# info:   constructor for zelig objects


# @formula: formula object used to create the model
# @model:   character-string specifying the model to run
# @data:    data.frame used to make sense of the formula
# @...:     any parameters that need to be sent to the model
# @cite:    boolean specifying whether to output citation information
# @depends: character-vector specifying model dependencies
#
# return:   a zelig object
zelig <- function (formula, model, data, ..., by=NULL, cite=T) {
  if (!missing(by)) {
    if (any(by %in% all.vars(formula))) {
      warning("by cannot list contain a variable from the model's formula")
      by <- NULL
    }

    if (length(by) > 1) {
      warning("by cannot have length greater than 1")
      by <- NULL
    }
  }

  # expand dot arguments
  dots <- list(...)

  # get non-dot arguments in a general fashion
  notdots <- as.list(match.call(expand.dots=F)[-1])
  notdots[["..."]] <- NULL

  # only get the non-dot arguments
  # that do not exist in the dot arguments
  names.notdots <- Filter(function(x) !x%in%names(dots), names(notdots))
  notdots <- notdots[names.notdots]

  # build parameter list (including optional parameters)
  params <- c(dots, notdots)

  # construct model object
  class(model) <- model

  # set up list
  res <- NULL
  old.style.oop <- TRUE

  # create a data.frame iterator
  m <- mi(data, by=by)

  # initialize variables for loop
  k <- 1
  res <- list()

  # repeat
  repeat {
    # get the next data.frame
    d.f <- try(nextElem(m), silent=T)

    # catch end-of-list error
    if (inherits(d.f, "try-error"))
      break

    zelig2 <- paste("zelig2", as.character(model), sep="")
    zelig2 <- get(zelig2, mode="function")
    zc.list <- zelig2(model, formula, ..., data=d.f)


    # create zelig.call
    zc <- ZeligCall(zc.list,
                    params = params
                    )

    # run the model
    new.res <- run(zc, data=d.f)

    # test
    old.style.oop <- ! isS4(new.res)

    # append to list
    res[[k]] <- new.res
    k <- k+1
  }

  # appropriately name each entry
  # (because both should be in order)
  
  # this is kludge.  can we (I) clean this up?
  if (!inherits(data, "mi") && is.null(by))
    res <- res[[1]]

  # run clean-up hooks on every result
  # ...


  # build zelig object
  z <- list(name    = as.character(model),
            formula = formula,
            result  = res,
            args    = list(...),
            data    = data,
            call    = match.call(),
            by      = by,
            mi      = reset(m),
            func    = zc.list[[1]],
            levels  = m$levels,
            S4      = !old.style.oop
            )

  # always attach the model name,
  # so that developers can overload
  class(z) <- c("zelig", model)

  # ...
  z$function.space <- if(old.style.oop)
    .RegisterMethodsS3(c("terms", register(z)))
  else
    .RegisterMethodsS4(c("terms", register(z)))

  # pre-pend "MI" class to sets of results
  if (is.list(z$result) && length(m) > 1)
    class(z) <- c("MI", class(z))
  else if (inherits(data, "mi"))
    class(z) <- c("MI", class(z))

  
  # citation
  if (cite) {
    descr <- as.description(describe(z))
    message(cite(descr))
  }

  z
}
