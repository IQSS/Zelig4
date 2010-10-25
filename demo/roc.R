data(turnout)
z.out1 <- zelig(vote ~ race + educate + age, model = "logit", 
                data = turnout)
user.prompt()
z.out2 <- zelig(vote ~ race + educate, model = "logit", 
                data = turnout)
user.prompt()
rocplot(z.out1$y, z.out2$y, fitted(z.out1), fitted(z.out2))
