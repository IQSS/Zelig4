#' Produce All Combinations of a Set of Lists
#' @note This function is used internall by the 'mi' constructors in order to
#'   produce the complete set of combinations of data-frames and factors by
#'   to subset the data-frames.
#' @param ... a set of lists to mix together
#' @return all the combinations of the lists with repetition
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


#' Construct a Subsetted Data-Frame
#' This mysteriously named function is used internally by Zelig to process
#' subsetted data-frames with the 'mi' function. In particular, it is primarily
#' used by mi's 'nextElem' method.
#' @note This function is exclusively used internally by Zelig.
#' @param dataf a data.frame
#' @param labels a vector of character-strings (currently ignored)
#' @return a data.frame with an overloaded operator
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zframe <- function(dataf, labels=NULL) {
  # build list
  z <- list(data=dataf)

  # set class, and return
  class(z) <- c("zframe", "data.frame")
  z
}

#' Extract Subsetted Data-Frames from \code{zframe} Objects
#' \code{zframe} objects are exclusively used internally within Zelig; they 
#' facilitate the process of subsetting data-frames for the \code{mi} function.
#' @note The replacement operator is undefined for \code{zframe} objects.
#' @param z a 'zframe' object used to describe multiple subsetted data-sets
#' @param ... parameters to be passed to the default extraction operator
#' @return a subsetted data-frame
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
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
      warning(key, "does not exist")
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

#' Extract the Data-Frame
#' @note This method is only intended for internal use by Zelig; its
#'   functionality lacks enough polish for interactive use by users or use by
#'   other software packages (without reading this warning).
#' @param x a 'zframe' object
#' @param row.names ignored
#' @param optional ignored
#' @param ... ignored
#' @return the corresponding subsetted data-frame
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.data.frame.zframe <- function(x, row.names=NULL, optional = FALSE, ...)
  x$data


#' Split a Parameter List into Two Lists
#' @note This is essentially a wrapper function for \link{splitUp}. This
#'   was used internally by Zelig, but now is primarily syntactical sugar for
#'   less important features.
#' @param ... a mix of parameters of any time
#' @return a list containing two entries: the key-value paired entires (titled
#'   wordful) and the unkeyed entried (titled wordless)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
#' @examples
#' chop.up(x=1, 2, "red", y=2)
#' #list(wordful = list(x=1, y=2), wordless=list(2, "red"))
#' @seealso splitUp
chop.up <- function(...)
  splitUp(list(...))

#' Split a List into Two Lists
#' This functions takes any list, and splits into two lists - one containing
#' the values of arguments with specifically specified values and those without
#' specified values.
#' @note This function is a good candidate for deprecation
#' @param args a list
#' @return a list containing two entries: the key-value paired entires (titled
#'   wordful) and the unkeyed entried (titled wordless)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
#' @examples
#' chop.up(list(x=1, 2, "red", y=2))
#' #list(wordful = list(x=1, y=2), wordless=list(2, "red"))
splitUp <- function(args) {
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


#' Convert a Return-value from a 'zelig2' Function into Meaningful Parameters
#' 
#'
# @param args a list of arguments with values that
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
    stop("zelig2 function did not return a list")

  #
  if (0 %in% nchar(names(args))) {
    warning("zelig2 function contains an entry without a key-value pair")
  }

  #
  args <- splitUp(args)$wordful

  #
  if (is.null(args$.function)) {
    stop("no `.function' specified in zelig2function")
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

#' Compute the Intersection of Two Sets
#' @note This function is used internally by Zelig
#' @param a a vector
#' @param b a vector
#' @param unique a boolean determining whether a intersect b will contain only
#'   unique elements
#' @return the intersection of a and b
.intersection <- function(a, b, unique=TRUE) {
  intersection <- a[a %in% b]

  if (unique)
    intersection <- unique(intersection)

  if (is.null(intersection))
    c()
  else
    intersection
}

#' Hook to Update the Zelig Call with the Appropriate Call Object
#' @note This function is used internally by Zelig, and currently deprecated.
#' @param zobj a 'zelig' object
#' @param call1 the original call to Zelig
#' @param call2 the manuafactured call to the model fitting function
#' @return the 'zelig' object with a modified 'call' slot
replace.call <- function(zobj, call1, call2) {
  # what if it doesn't exist?
  if (!is.null(zobj$result$call) && is.call(zobj$result$call2))
    zobj$result$call <- call2

  zobj
}

#' Wether an Installed R-Pack Depends on Zelig
#' @note This package was used internally to determine whether an R-package is
#'   Zelig compliant, but is now likely deprecated. This test is useless if not
#'   paired with 
#' @param package a character-string naming a package
#' @return whether this package depends on Zelig
is.zelig.package <- function(package="") {
  "Zelig" %in% tools:::pkgDepends(package)$Depends
}

#' Whether a R-Package Contains a 'Yes' in its DESCRIPTION File's 'Zelig' Field
#' @note This package was used internally to determine whether an R-package is
#'   Zelig compliant, but is now likely deprecated.
#' @param package a character-string specifying an installed R-package
#' @return whether the package's DESCRIPTION file specifies Zelig-compliancy
#' @seealso is.zelig.package
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
is.zelig.compliant <- function(package="") {
  #
  zcomp <- packageDescription(package, fields="Zelig-Compliant")
  zcomp <- tolower(zcomp)

  #

  if (! zcomp %in% c('yes', 'no'))
    stop("")

  zcomp == "yes"
}
