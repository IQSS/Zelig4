data(mexico)
user.prompt()
z.out1 <- zelig(as.factor(vote88) ~ pristr + othcok + othsocok, model = "mlogit", 
               data = mexico)
user.prompt()
print(summary(z.out1))

user.prompt()
x.weak <- setx(z.out1, pristr = 1)
x.strong <- setx(z.out1, pristr = 3)

user.prompt()
s.out1 <- sim(z.out1, x = x.strong, x1 = x.weak)
user.prompt()
print(summary(s.out1))

user.prompt()
ev.weak <- s.out1$qi$ev + s.out1$qi$fd

user.prompt()
ternaryplot(s.out1$qi$ev, pch = ".", col = "blue",
            main = "1988 Mexican Presidential Election")
user.prompt()
ternarypoints(ev.weak, pch = ".", col = "red")

# Specifying different sets of explanatory variables for each factor level
user.prompt()
z.out2 <- zelig(list(id(vote88,"1")~pristr + othcok, id(vote88,"2")~othsocok), model = "mlogit", 
               data = mexico)
user.prompt()
print(summary(z.out2))

user.prompt()
x.weak <- setx(z.out2, pristr = 1)
x.strong <- setx(z.out2, pristr = 3)

user.prompt()
s.out2 <- sim(z.out2, x = x.strong, x1 = x.weak)
user.prompt()
print(summary(s.out2))

user.prompt()
ev.weak <- s.out2$qi$ev + s.out2$qi$fd

user.prompt()
ternaryplot(s.out2$qi$ev, pch = ".", col = "blue",
            main = "1988 Mexican Presidential Election")
user.prompt()
ternarypoints(ev.weak, pch = ".", col = "red")
