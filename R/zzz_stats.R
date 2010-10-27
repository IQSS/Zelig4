# @author: Matt Owen
# @date:   5/24/2010
# @file:   stats.zelig.R

# @x:     a set of data
# return: the statistical mode of the set
mode.zelig <- function (x) {
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


# @x:     
# return: the median
median.zelig <- function (x) {
  v <- ifelse(is.numeric(x),
              median(v),
              levels(x)[ceiling(median(as.numeric(x)))]
              )
  if (is.ordered(x))
    v <- factor(v, levels(x))
  v
}

# @x:     
# return: 
max.zelig <- function (x) {
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
    stop("Error: max cannot be computed for this data-type.")
}

# @x: asdads
# return:
min.zelig <- function (x) {
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
    stop("Error: min cannot be computed for this data-type.")
}
