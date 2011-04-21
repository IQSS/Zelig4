#' @S3method sim MI
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
sim.MI <- function(obj, x = NULL, x1 = NULL, y = NULL, num = 1000) {

  message("???")

  sim.results <- list()

  for (key in names(obj$list)) {

    message(key)

    object <- obj$list[[key]]
    new.x <- x[[key]]
    new.x1 <- x1[[key]]

    print(class(new.x))

    sim.results[[key]] <- sim(object, x=new.x, x1=new.x1, y=new.y, num=num)
    print(summary(sim.results[[key]]))
  }

  sim.results
}

