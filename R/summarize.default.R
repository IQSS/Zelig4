#' Summarize simualted quantities of interest
#' 
#' @S3method summarize default
#'
#' @param q a `qi' object, storing simulations of quantities of interest
#' @return a `summarized.qi' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summarize.default <- function(q) {
  i <- iter(q)
  res <- list()


  repeat {
    item <- try(nextElem(i), silent=T)

    # conditions end repeat loop
    if (inherits(item, "try-error"))
      break

 
    # for code clarity
    key <- item$key
    val <- item$value


    # conditions to skip a qi
    if (!is.qi(val))
      next

    else if (all(is.na(val)))
      next
    
    # make a matrix that is data-friendly
    m <- if (is.numeric(val)) {
      matrix(NA, nrow=ncol(val), ncol=5)
    }
    else if (is.character(val) || is.factor(val)) {
      levels <- levels(val)

      if (is.null(levels)) {
        #warning("Indeterminate number of levels for qi: ", key)
        unique(c(val))
      }

      levels <- sort(levels)

      matrix(NA, nrow=ncol(val), ncol=length(levels), dimnames=list(NULL, levels))
    }

    #
    for (k in 1:ncol(val)) {
      if (is.numeric(val[,k])) {
        row <-c(
                mean(val[,k], na.rm = TRUE),
                sd(val[,k], na.rm = TRUE),
                quantile(val[,k], c(.5, .025, .975), na.rm=TRUE)
                ) 
        m[k,] <- row


        #
        colnames(m) <- c("mean", "sd", "50%", "2.5%", "97.5%")
      }
    
      else if (is.character(val[,k]) || is.factor(val[,k])) {

        # A table specifying the _percentage_ of simulations matching
        # each particular level of the factor qi's
        result.table <- table.levels(val[,k], levels = levels)
        result.table <- result.table/length(val[,k])

        # A character-vector specifying the factors found in the qi
        factor.names <- sort(names(result.table))

        # This should prevent size errors for qi's with
        # a NULL levels attribute
        # in particular, it resolves issues 
        m[k, ] <- 0
        m[k, factor.names] <- result.table[factor.names]

        m[k,] <- result.table
        colnames(m) <- names(result.table)
      }

      else
        m[k,] <- NA


      col.names <- colnames(val)
      rownames(m) <- if (is.null(col.names))
        ""
      else
        col.names
    }

    # add to list
    res[[key]] <- m
  }

  # cast as class - for some reason - then return
  class(res) <- "summarized.qi"
  res
}


#' Test If Value is Interpretable as a QI
#' @param qi a potential quantity of interest
#' @return a logical specifying whether this value should or should-not
#'         be output
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
is.qi <- function(qi) {
  if (is.null(qi))
    FALSE

  if (!length(qi))
    FALSE

  if (all(is.na(qi)))
    FALSE

  TRUE
}


#' Create a table, but ensure that the correct
#' columns exist. In particular, this allows for
#' entires with zero as a value, which is not
#' the default for standard tables
#' @param x a vector
#' @param levels a vector of levels
#' @param ... parameters for table
#' @return a table
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
table.levels <- function (x, levels, ...) {
  # if levels are not explicitly set, then
  # search inside of x
  if (missing(levels)) {
    levels <- attr(x, 'levels')
    table(factor(x, levels=levels), ...)
  }

  # otherwise just do the normal thing
  else {
    table(factor(x, levels=levels), ...)
  }
}


#' @S3method iter summarized.qi
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
iter.summarized.qi <- function(s) {
  iter(Map(function (x, y) list(key=x, value=y), names(s), s))
}


#' @S3method iter qi
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
iter.qi <- function(s) {
  lis <- list()
  indices <- names(attr(s, '.index'))

  for (index in indices)
    lis[[index]] <- list(key = index, value = s[[index]])

  iter(lis)
}
