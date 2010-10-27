#
.GetGenerics <- function(...) UseMethod(".GetGenerics")

# needs work
.GetGenerics.MI <- function(...) new.env()

# @zelig.object: a zelig object
# @envir:        namespace to search with 'ls'
# return:        a list of generic functions names to
#                to define for zelig
.GetGenerics.default <- function(zelig.object, envir=parent.frame()) {
  if (is.null(zelig.object$S4))
    stop(as.character(zelig.object$family[[1]]))
  else if (zelig.object$S4)
    .GetGenericsS4(zelig.object, envir)
  else
    .GetGenericsS3(zelig.object, envir)
}

.GetGenericsS3 <- function(zelig.object, envir=parent.frame()) {
  #
  hash <- list()
  cls <- class(zelig.object$result)
  method.list <- as.character(methods(class=cls))
  method.list <- gsub(paste("\\.", cls, "$", sep=""), "",
                      method.list)


  meth.list <- c()
  for (cl in c(class(zelig.object$result), "default")) {
    method.list <- as.character(methods(class=cl))
    method.list <- gsub(paste("\\.", cl, "$", sep=""), "", method.list)
    meth.list <- unique(c(meth.list, method.list))
  }

  # final list
  flist <- c("zelig", "param", "as.parameters", "sim", "setx", "register")
  meth.list <- sort(unique(c(meth.list, names(get(".knownS3Generics")))))
  meth.list %w/o% flist
}


# @name:  name of generic function
# return: a function that calls the given generic 
#         function on the result object
.NewZeligGenericS3 <- function(name) {
  # store name in this environment
  stored.name <- name

  # return function
  function (...) {
    # get list of parameters,
    # omitting function name
    params <- as.list(match.call())[-1]

    # get the result object from the zelig object
    params[[1]] <- ..1$result

    # call function on the fitted model
    do.call(stored.name, params)
  }
}


# @object: a zelig object
# @lis: a character-vector of names to register for zelig objects
.RegisterMethods <- function(lis)
  .RegisterMethodsS3(lis)



# @lis: 
# return: 
.RegisterMethodsS3 <- function(lis) {
  # error-catching & handling
  saved.environment <- new.env()

  # iterate through the list
  for (meth in lis) {
    # assign the relevant function in the
    # correct environment or namespace
    assign(paste(meth, "zelig", sep="."),
           .NewZeligGenericS3(meth),
           envir = .GlobalEnv
           )
  }

  # return
  invisible(saved.environment)
}

# @f: a character-string specifying the function name
# @class: a character-string specifying the class name
.existsS3method <- function(f, class, ...)
  tryCatch(is.function(getS3method(f, class, ...)),
           error=function (e) FALSE
           )
