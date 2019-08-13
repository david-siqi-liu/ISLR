# 3.6.1 - Install & load libraries

library(MASS)
#install.packages("ISLR")
library(ISLR)

# 3.6.2 - Simple linear regression
# Predict the `medv` (median house value)
View(Boston)
names(Boston)
?Boston

# Fit `medv` using lstat` (lower status of the population)
lm.fit=lm(medv~lstat, data=Boston)
lm.fit
summary(lm.fit)
names(lm.fit) # Get attributes
lm.fit$coefficients
coef(lm.fit) # Preferred

# Confidence interval
confint(lm.fit)

# Prediction given value of `lstat`
# As expected, PI is substrantially wider than CI
predict(lm.fit,
        data.frame(lstat=c(5,10,15)),
        interval="confidence") # CI
predict(lm.fit,
        data.frame(lstat=c(5,10,15)),
        interval="prediction") # PI

# Plot
attach(Boston)
plot(lstat, medv, col="red", pch="+")
abline(lm.fit) # Draws the least squares regression line

# Plot experimentations

