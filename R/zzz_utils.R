# @...:   a set of lists to mix together
# return: all the combinations with repetition
#
# **note: maybe can be done better with recursion?
#         I think it might not got better than this
#         without having specific patterns known
.combine <- function(...) {
  # expand dot arguments
  dots <- list(...)

  # error-catching
  if (length(dots) < 1)
    return(NULL)

  # prepare lists for first iteration
  res <- dots[[1]]
  dots <- dots[-1]

  # this entire algorithm could be optimized,
  # however, it will always be exponential time
  while(length(dots) > 0) {
    # get list to store new combinations in
    new.list <- list()

    # divide list
    first <- dots[[1]]

    # add new combinations
    for (f in first)
      for (r in res)
        new.list[['']] <- c(r, f)

    # update list
    res <- new.list

    # shift first entry off
    dots <- dots[-1]
  }

  # m, as in matrix
  m <- NULL

  # format results as a matrix
  for (r in res)
    m <- rbind(m, r)

  # name rows/cols
  rownames(m) <- 1:length(res)
  colnames(m) <- names(list(...))

  # return
  m
}


# @dataf: a data.frame
# return: a data.frame with an overloaded operator
zframe <- function(dataf, labels=NULL) {
  # build list
  z <- list(data=dataf)

  # set class, and return
  class(z) <- c("zframe", "data.frame")
  z
}

"[.zframe" <- function(z, ...) {
  # expand dots
  dots <- list(...)

  # get keys
  keys <- Filter(nchar, names(dots))

  # but what if there's a list passed in?
  if (is.list(..1)) {
    dots <- ..1
    keys <- Filter(nchar, names(..1))
  }

  # get the data.frame
  zef <- z$data

  # if no keywords are specified, then 
  # we want to use the standard data.frame method
  if (length(keys) < 1)
    NextMethod(generic="[", object=zef)

  # filter each key-value pair
  for (key in keys) {
    # warn if a specified key doesn't exist
    # then skip
    if (! key %in% names(zef)) {
      warning()
      next
    }

    # get value to filter for
    val <- dots[[key]]

    # filter
     zef <- zef[zef[,key] == val,]
  }

  # return
  zef
}

#
as.data.frame.zframe <- function(zef)
  zef$data


#
chop.up <- function(...) split.up(list(...))

split.up <- function(args) {
  wordless <- list()
  wordful <- list()

  k <- 1

  if (is.null(names(args)))
    return(list(wordless=unlist(args), wordfull=NULL))

  for (key in names(args)) {
    if (nchar(key) == 0)
      wordless <- c(wordless, args[[k]])
    else
      wordful[[key]] <- args[[k]]

    k <- k+1
  }

  list(wordless=wordless, wordful=wordful)
}


# @args: a list of arguments with values that
#        are ALREADY evaluated
# return: a list organized that divides zelig-
#         parameters from model parameters
#
# NOTE: zelig-parameters are prefixed with a
#       dot, so as to avoid conflicts with
#       standard variable naming conventions
#
# PS: this function primarily is an error-catcher,
#     and thought organizer
.zelig2ify <- function(args) {
  #
  if (!is.list(args))
    stop("How about I'm outside your window")

  #
  if (0 %in% nchar(names(args))) {
    warning()
  }

  #
  args <- split.up(args)$wordful

  #
  if (is.null(args$.function)) {
    stop()
  }

  # move to variables
  model.func <- args$.function
  hook.func  <- args$.hook
  final.func <- args$.final
  mi.func <- args$.mi

  # remove zelig-parameters from args
  args$.function <- NULL
  args$.hook <- NULL
  args$.final <- NULL
  args$.mi <- NULL

  # return array
  # NOTE: arguably, this should be an object,
  #       but that seems like a lot of effort
  #       for something that is only used once
  list(# necessary functions
       .function = (substitute(model.func)),

       # optional functions
       .hook     = hook.func,
       .final    = final.func,
       .mi = mi.func,

       # parameter list
       parameters = args
       )
}


# @topic: character-string representing help-topic
# @package: package containing help-topic
# return: character-string of processed Rd file
.get.help.file <- function(topic, package) {
  # get package help-file if no topic is set
  if (missing(topic))
    topic <- package
  
  # error-checking:
  #   ensure file and package are strings
  if (!is.character(topic) && length(topic) > 1L)
    stop()

  if (!is.character(package) && length(package) > 1L)
    stop()

  # 
  directory <- system.file(package=package)

  # 
  path <- utils:::index.search(
                               topic=topic,
                               paths=directory
                               )

  # search package-help-dataabase, get Rd file as string
  utils:::.getHelpFile(file=path)
}



# @package: character-string specifying the name of a package to
#           scan for help files
# @as.table: boolean specifying whether the return value will be
#            a table or names of Rd files
# return: either a named vector (table), or an unnamed vector
.list.help.files <- function(package, as.table=TRUE) {
  # index for help files
  fi <- file.path(
                  system.file(package=package),
                  "help",
                  "AnIndex"
                  )

  if (file.exists(fi)) {
    # get index of search-values and corresponding
    #  Rd file
    index <- scan(fi,
                  what = list(names="", values=""),
                  sep = "\t",
                  quote = "",
                  na.strings = "",
                  quiet = TRUE
                  )

    # the if-else below is a return value
    if (as.table)
      # return as an index
      structure(index$values, names=index$names)
    
    else
      # return only the names of the Rd files
      index$names
  }
  else {
    warning("nothing was found")
    NULL
  }
}



# @a: a vector
# @b: a vector
# @unique: a boolean determining whether a intersect b
#          will contain only unique elements
# return: the intersection of a and b
.intersection <- function(a, b, unique=TRUE) {
  intersection <- a[a %in% b]

  if (unique)
    intersection <- unique(intersection)

  if (is.null(intersection))
    c()
  else
    intersection
}


replace.call <- function(zobj, call1, call2) {
  # what if it doesn't exist?
  if (!is.null(zobj$result$call) && is.call(zobj$result$call2))
    zobj$result$call <- call2

  zobj
}


# @package: a character-string naming a package
is.zelig.package <- function(package="") {
  "Zelig" %in% tools:::pkgDepends(package)$Depends
}
