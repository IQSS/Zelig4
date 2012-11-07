library(Zelig)

data(klein)

formula <- list(
                mu1  = C ~ Wtot + P1,
                mu2  = I ~ P + P1 + K1,
                mu3  = Wp ~ X + X1 + Tm,
                inst = ~ P1 + K1 + X1 + Tm + Wg + G
                )

z <- zelig(formula, model="twosls", data=klein, cite=F)

x <-setx(z)
x1 <-setx(z, Wtot = 60)

s <-sim(z, x, x1)

summary(s)

# Plot
plot(s)
