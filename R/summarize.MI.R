#' Summarize simualted quantities of interest for multiply-imputed data
#' 
#' @S3method summarize default
#'
#' @param qis a `qi' object, storing simulations of quantities of interest
#' @return a `summarized.qi' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summarize.MI <- function(qis) {

  summ.list <- list()

  for (q in qis$results) {
    message("#")
    # assig correct class
    class(q) <- class(q)[ ! class(q) %in% "MI" ]

    # get summary data for individual qi    
    summ <- summarize(q)

    # bind rows, so that data is organized correctly
    for (key in names(summ))
      summ.list[[key]] <- rbind(summ.list[[key]], summ[[key]])
  }

  # name rows appropriately
  for (key in names(summ.list)) {
    # add filler as needed
    gap <- nrow(summ.list[[key]])/length(qis$levels) - 1

    #
    data.labels <- paste(.space.out(qis$levels, gap=gap),
                         rownames(summ.list[[key]]),
                         sep=" "
                         )

    # right-justify row names
    data.labels <- format(data.labels, justify="right")

    #
    rownames(summ.list[[key]]) <- paste(data.labels, ": ", sep="")

    # ...
    # summ.list[[key]] <- summ.list[[key]][sort(rownames(summ.list[[key]])),]
  }

  # assign class, for some reason
  class(summ.list) <- "summarized.qi"

  print(summ.list)

  # return
  summ.list
}


.space.out <- function(lis, gap=1, fill="", trailing=TRUE) {
  # ignore weird values for gap
  if (gap < 1)
    return(lis)

  gap <- as.integer(gap)

  trail <- as.integer(!trailing)
  
  len <- length(lis)
  res <- vector("character", length=len + gap*(len-trail))

  assign.vector <- (1:len)*(gap+1)-gap
  res[assign.vector] <- lis
  res[-assign.vector] <- fill
  res
}
