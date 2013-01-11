library(Zelig)
library(Amelia)


# Create data set

beta <- c(.3, -10)

.x1 <- runif(1000, -5, 5)
.x2 <- runif(1000, -2, 2)
.x3 <- sample(1:4, 1000, TRUE)
.y <- t(beta %*% rbind(.x1 + rnorm(1000, 0, 1.2), .x2 + rnorm(1000, 0, .1))) + 3 + rnorm(1000, 0, .3)

data.set <- data.frame(y = .y, x1 = .x1, x2 = .x2, x3 = .x3)

# Add missing data

missing.data.percent <- .3
missing.data.column <- "x1"
missing.data.rows <- sample(1:nrow(data.set), round(missing.data.percent * nrow(data.set)))

data.set[missing.data.rows, missing.data.column] <- NA

# Impute

imputed.data <- amelia(data.set)

# Remove unused data sets

rm(.y, .x1, .x2)

# Print amelia obj

imputed.data

# Fit statistical model

z <- zelig(y ~ x1 + x2, model = "ls", data = imputed.data)
x <- setx(z)
s <- sim(z, x)

#

summary(s)

# Fin.
