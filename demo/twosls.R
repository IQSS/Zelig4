data(klein)

formula <- list(
                mu1 = C ~ Wtot + P1,
                mu2 = I ~ P + P1 + K1,
                mu3 = Wp ~ X + X1 + Tm,
                inst= ~ P1 + K1 + X1 + Tm + Wg + G
                )

z.out<-zelig(formula=formula, model="twosls",data=klein, cite=F)

x.out <-setx(z.out)

s.out <-sim(z.out,x=x.out)

summary(s.out)


# Plot

user.prompt()
plot(s.out)


