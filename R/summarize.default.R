#' Summarize Simualted Quantities of Interest
#'
#' @usage \method{summarize}{default}(obj)
#' @S3method summarize default
#' @param obj a \code{qi} object, storing simulations of quantities of interest
#' @return a 'summarized.qi' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summarize.default <- function(obj) {
  res <- list()
  titles <- names(attr(obj, ".index"))

  for (key in titles) {
    val <- obj[[key]]

    if (!is.qi(val))
      next

    if (!is.matrix(val))
      val <- matrix(val, ncol=1, nrow=length(val))

    
    # make a matrix that is data-friendly
    m <- if (is.numeric(val)) {
      matrix(NA, nrow=ncol(val), ncol=5)
    }
    else if (is.character(val) || is.factor(val)) {
      levels <- levels(val)

      if (is.null(levels)) {
        #warning("Indeterminate number of levels for qi: ", key)
        levels <- unique(c(val))
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
    return(FALSE)

  else if (!length(qi))
    return(FALSE)

  else if (all(is.na(qi)))
    return(FALSE)

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
