summarize.MI <- function(qis) {
  summ.list <- list()

  for (q in qis$results) {
    # get summary data for individual qi
    summ <- summarize(q)

    # bind rows, so that data is organized correctly
    for (key in names(summ))
      summ.list[[key]] <- rbind(summ.list[[key]], summ[[key]])
  }

  # name rows appropriately
  for (key in names(summ.list)) {
    rownames(summ.list[[key]]) <- paste(qis$levels, ": ", sep="")
    summ.list[[key]] <- summ.list[[key]][sort(rownames(summ.list[[key]])),]
  }

  # assign class, for some reason
  class(summ.list) <- "summarized.qi"

  # return
  summ.list
}
