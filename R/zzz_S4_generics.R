# @lis: list of characters
.RegisterMethodsS4 <- function(lis) {
  # create a new environment
  saved.env <- new.env()

  #
  for (fname in lis) {
    # Register Method S4
    # not: Register Methods S4
    # this is not recursive!
    .RegisterMethodS4(fname)
  }

  saved.env
}


# @name: character-string
.RegisterMethodS4 <- function(name) {
  res <- try(setMethod(name, signature="zelig",
                       where = .GlobalEnv,
                       definition=.NewZeligGenericS4(name)
                       ),
             silent=TRUE
             )

  res <- try(setMethod(name, signature="MI",
                       where = .GlobalEnv,
                       definition=.NewZeligMIGenericS4(name)
                       ),
             silent=FALSE
             )

  if (inherits(res, "try-error")) {
    FALSE
  }

  else {
    res
  }
}


# @name: name of the generic
# return: anonymous function
# **************************
# note that the following function changes the
# formal arguments to that of the original
# generic function.  this is to suppress most
# warnings that crop up while setting an S4 method
.NewZeligGenericS4 <- function(name) {
  # get generic
  fdef <- getGeneric(name)

  #
  fformals <- formals(fdef)
  stored.name <- name

  # 
  f <- function(object, ...) {
    # get list of parameters,
    # ommitting function name
    params <- as.list(sys.call())[-1]

    # get the result object from the zelig object
    params[[1]] <- eval.parent(params[[1]])$result

    # call the function on the fitted model
    do.call(name, params)
  }

  # assign formals, so that they match perfectly
  formals(f) <- fformals

  # return
  f
}

#
#
.NewZeligMIGenericS4 <- function(name) {
  # get generic
  fdef <- getGeneric(name)

  #
  fformals <- formals(fdef)
  stored.name <- name

  # define function
  f <- function(object, ...) {
    params <- as.list(sys.call())
    params[[1]] <- stored.name
    params[[2]] <- eval.parent(params[[2]])$result

    #
    do.call(Map, params)
  }

  formals(f) <- fformals

  # return
  f
}



# @object: a zelig object
.GetGenericsS4 <- function(object, envir=parent.frame()) {
  if (inherits(object$result, "list")) {
    .ListS4Generics(classes=class(object$result[[1]]), env=env)
  }
  else
    .ListS4Generics(classes=class(object$result), env=env)
}


# @classes: classes
# @where: compatibility with showMethods
# @env: the environment to search for generics
# return: a character-vector of function names
# ********************************************
# this function searches .AllMTable within the namespace
# of the functions environment
.ListS4Generics <- function(classes=NULL, where=NULL,
                          env=topenv(parent.frame())) {
  # get list of all generic functions
  functions <- if (missing(where))
    getGenerics()
  else
    getGenerics(where)

  #
  matches <- c()
  functions <- as.character(functions)

  #
  for (f in functions) {
    fdef <- getGeneric(f)
    env <- environment(fdef)

    table <- get(".AllMTable", envir=env)

    if (any(classes %in% ls(table)))
      matches <- append(matches, f)
  }

  # return
  flist <- c("zelig", "param", "as.parameters", "sim", "setx", "register", 'summary')
  matches[ ! matches %in% flist ]
}

