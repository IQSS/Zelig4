#
.GetGenerics <- function(...) UseMethod(".GetGenerics")

# needs work
.GetGenerics.MI <- function(...) new.env()

# @zelig.object: a zelig object
# @envir:        namespace to search with 'ls'
# return:        a list of generic functions names to
#                to define for zelig
.GetGenerics.default <- function(zelig.object, envir=parent.frame()) {
  #
  hash <- list()
  method.list <- as.character(methods(class="glm"))
  method.list <- gsub(paste("\\.", 'glm', "$", sep=""), "",
                      method.list)


  meth.list <- c()
  for (cl in c(class(zelig.object$result), "default")) {
    method.list <- as.character(methods(class=cl))
    method.list <- gsub(paste("\\.", cl, "$", sep=""), "", method.list)
    meth.list <- unique(c(meth.list, method.list))
  }

  # final list
  flist <- c("zelig", "param", "as.parameters", "sim", "setx")
  meth.list <- sort(unique(c(meth.list, names(get(".knownS3Generics")))))
  meth.list <- meth.list[! meth.list %in% flist]
  meth.list
}


# @zelig.object: a zelig object
# return: environment containing defined functions
.MakeGenerics <- function(zelig.object) {
  #
  func <- zelig.object$func

  # error-catching & handling
  if (is.name(func))
    func <- as.character(func)

  if (is.character(func) && exists(func, mode="function"))
    func <- get(func, mode="function")
  
  else if (!is.function(func) && !is.character(func))
    stop("no function?")

  # 
  saved.environment <- new.env()

  # iterate through the list
  for (meth in .GetGenerics(zelig.object)) {
    # assign the relevant function in the
    # correct environment or namespace
    assign(paste(meth, "zelig", sep="."),
           .NewZeligGeneric(meth),
           envir = .GlobalEnv
           )
  }

  # return
  invisible(saved.environment)
}

# @name:  name of generic function
# return: a function that calls the given generic 
#         function on the result object
.NewZeligGeneric <- function(name) {
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

# @lis:
# return:
.RegisterMethods <- function(lis) {
  # error-catching & handling
  saved.environment <- new.env()

  # iterate through the list
  for (meth in lis) {
    # assign the relevant function in the
    # correct environment or namespace
    assign(paste(meth, "zelig", sep="."),
           .NewZeligGeneric(meth),
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
