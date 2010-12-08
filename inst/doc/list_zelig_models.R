library(Zelig)

fi <- file("make_docsIII.conf", "w")

writeLines(ZeligListTitles(), con=fi)

close(fi)
