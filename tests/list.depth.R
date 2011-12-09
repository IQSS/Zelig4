library(Zelig)

list.depth <- Zelig:::list.depth


list.depth(NULL)
list.depth(list(1, list(list(list(1)))))
list.depth(list(1))
list.depth(list(1, list(NA), 2))
list.depth(call("sin", x=3))
list.depth(list(call("sin", x=3)))
