library(Zelig)

# get lists
zelig.objects <- ls(envir=asNamespace("Zelig"))
zelig.help <- Zelig:::.list.help.files("Zelig", as.table=FALSE)

# look at intersection
available.files <- Zelig:::.intersection(zelig.objects, zelig.help)


# directory to write to
directory <- "Rd2TeX"
dir.create(directory, showWarnings=FALSE)

# 
for (rd in available.files) {
  # store Rd document as a string
  str <- Zelig:::.get.help.file(rd, package="Zelig")

  #
  file.name <- paste(rd, "tex", sep=".")

  #
  path <- file.path(directory, file.name)


  #
  unlink(file.name)

  
  # open file handle
  file.handle <- file(path, "w+")

  #
  tools:::Rd2latex(str, file.handle)

  # close handle
  close(file.handle)
}
