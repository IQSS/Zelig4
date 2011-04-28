#' Compute the Statistical Mode of a Vector
#' @param x a vector of numeric, factor, or ordered values
#' @return the statistical mode of the vector. If two modes exist, one is
#'   randomly selected (by design)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Mode <- function (x) {
  # build a table of values of x
  tab <- table(as.factor(x))

  # find the mode, then if there's more than one, select one randomly
  v <- sample(names(which(tab == max(tab))))

  # if it came in as a factor, we need to re-cast it
  # as a factor, with the same exact levels
  if (is.factor(x))
    return(factor(v, levels=levels(x)))

  # re-cast as any other data-type
  as(v, class(x))
}


#' Compute the Statistical Median of a Vector
#' @param x a vector of numeric or ordered values
#' @param na.rm ignored
#' @return the median of the vector
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Median <- function (x, na.rm=NULL) {
  v <- ifelse(is.numeric(x),
              median(v),
              levels(x)[ceiling(median(as.numeric(x)))]
              )
  if (is.ordered(x))
    v <- factor(v, levels(x))
  v
}

#' Compute the Maximum Value of a Vector
#' @param x a numeric or ordered vector
#' @param na.rm ignored
#' @return the maximum value of the vector
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Max <- function (x, na.rm=NULL) {
  if (is.numeric(x))
    return(max(x))
  
  else if (is.ordered(x))
    return(factor(max(levels(x),
                      na.rm=T
                      ),
                  levels=levels(x)
                  )
           )

  else
    stop("Error: max cannot be computed for non-numeric and non-ordered values")
}

#' Compute the Minumum Value of a Vector
#' @param x a vector of numeric or ordered values
#' @param na.rm ignored
#' @return the minimum value of the vector
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
Min <- function (x, na.rm=NULL) {
  if (is.numeric(x))
    return(min(x))
  
  else if (is.ordered(x))
    return(factor(min(levels(x),
                      na.rm=T
                      ),
                  levels=levels(x)
                  )
           )

  else
    stop("Error: min cannot be computed for non-numeric and non-ordered values")
}
