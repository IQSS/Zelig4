print.summary.glm.robust <-
    function (x, digits = max(3, getOption("digits") - 3),
	      symbolic.cor = x$symbolic.cor,
	      signif.stars = getOption("show.signif.stars"), ...)
{
  class(x) <- "summary.glm"
  print(x)
  cat("\nRobust standard errors computed using", x$robust)
  cat("\n")
  invisible(x)
}

