#' Produce All Combinations of a Set of Lists
#' @note This function is used internall by the 'mi' constructors in order to
#' produce the complete set of combinations of data-frames and factors by to
#' subset the data-frames.
#' @param ... a set of lists to mix together
#' @return all the combinations of the lists with repetition
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
mix <- function(...) {
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
      for (r in res) {
        row <- append(as.list(r), f)
        names(row) <- names(list(...))
        new.list[['']] <- row
      }

    # Update list
    res <- new.list

    # Shift first entry off
    dots <- dots[-1]
  }

  res
}
#' Produce All Combinations of a Set of Lists
#' @note This function is used internall by the 'mi' constructors in order to
#'   produce the complete set of combinations of data-frames and factors by
#'   to subset the data-frames.
#' @param ... a set of lists to mix together
#' @return all the combinations of the lists with repetition
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
combine <- function(...) {
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
