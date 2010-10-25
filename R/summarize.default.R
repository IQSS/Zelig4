summarize.default <- function(q) {
  # error-catching
  if (is.null(q$titles) || is.null(q$stats))
    stop("qi function missing qi's or their titles")


  #
  i <- iter(q)
  res <- list()


  repeat {
    item <- try(nextElem(i), silent=T)

    if (inherits(item, "try-error"))
      break

    # for code clarity
    key <- item$key
    val <- item$value

    #
    if (is.numeric(val)) {
      res[[key]] <- c(mean(val),
                      sd(val),
                      quantile(val, c(.5, .025, .975))
                      )
      names(res[[key]]) <- c("mean", "sd", "50%", "2.5%", "97.5%")
    }
    
    else if (is.character(val) || is.factor(val))
      res[[key]] <- c(table(val)/length(val))

    else
      res[[key]] <- NA
  }

  # cast as class - for some reason - then return
  class(res) <- "summarized.qi"
  res
}

iter.summarized.qi <- function(s)
  iter(Map(function (x, y) list(key=x, value=y), names(s), s))
