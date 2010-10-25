print.setx.mi <- function(x) {
  #
  message()
  message("  setx.mi: A COLLECTION OF setx OBJECTS")
  
  for (key in names(x$s.x)) {
    # create string
    st <- paste(x$by, "=", key)

    message("-------------------------------------------")
    message()
    message("display setx object for condition where:")
    message(st)
    print(x$s.x[[key]])
    message("===========================================")
    message()
  }
}
