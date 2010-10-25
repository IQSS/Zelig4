describe.zelig <- function(zelig.obj) {

  append(list(model=zelig.obj$name),
         NextMethod("describe")
         )
}
