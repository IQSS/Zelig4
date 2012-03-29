library(Zelig)

data(coalition)

# Fit statistical model
z.out <- zelig(duration ~ fract + numst2 + crisis,
               model = "gamma", 
               data = coalition
               )

# Set explanatory variables
x.low <- setx(z.out, fract=300, numst2 = 0, crisis=200)
x.high <- setx(z.out, fract=300, numst2 = 1, crisis=200)

# Simulate values
s.out <- sim(z.out, x = x.low, x1 = x.high, num = 20, bootstrap = T)

# Display the summary data
summary(s.out)
