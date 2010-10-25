gsource <- function(var.names= NULL, variables) {
  if (is.null(var.names)) {
    cat(variables, file = ".Rtmp.dat", sep = "\n")
    out <- read.table(".Rtmp.dat")
  }
  else {
    cat(var.names, variables, file = ".Rtmp.dat", sep = "\n")
    out <- read.table(".Rtmp.dat", header = TRUE)
  }
  unlink(".Rtmp.dat")
  return(as.data.frame(out))
}



