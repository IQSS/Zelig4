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
    if (length(val) < 1)
      next

    else if (length(val) == 1 && is.na(val))
      next

    else if (all(is.na(val)))
      next

    
    # make a matrix that is data-friendly
    m <- if (is.numeric(val))
      matrix(NA, nrow=ncol(val), ncol=5)
    else if (is.character(val) || is.factor(val)) {
      matrix(NA, nrow=ncol(val), ncol=length(unique(c(val))))
    }

    #
    for (k in 1:ncol(val)) {
      if (is.numeric(val[,k])) {
        m[k,] <- c(
                   mean(val[,k]),
                   sd(val[,k]),
                   quantile(val[,k], c(.5, .025, .975))
                   )

        #
        colnames(m) <- c("mean", "sd", "50%", "2.5%", "97.5%")
      }
    
      else if (is.character(val[,k]) || is.factor(val[,k])) {
        result.table <- c(table.levels(val[,k], levels = levels(val))/length(val[,k]))
        result.table <- result.table[sort(names(result.table))]

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

#
#
.result.table <- function () {

}


#' Create a table, but ensure that the correct
#' columns exist. In particular, this allows for
#' entires with zero as a value, which is not
#' the default for standard tables
#' param x a vector
#' param levels a vector of levels
#' param ... parameters for table
#" value a table
table.levels <- function (x, levels, ...) {
  # if levels are not explicitly set, then
  # search inside of x
  if (missing(levels)) {
    levels <- attr(x, 'levels')
    table(factor(x, levels=levels), ...)
  }

  # otherwise just do the normal thing
  else
    table(factor(x), ...)
}


iter.summarized.qi <- function(s)
  iter(Map(function (x, y) list(key=x, value=y), names(s), s))
