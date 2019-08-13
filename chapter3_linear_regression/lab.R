#' ---
#' title: "Chapter 3 - Linear Regression"
#' author: "David Liu"
#' ---

### 3.6.1 - Install & load libraries

library(MASS)
library(ISLR)

### 3.6.2 - Simple linear regression

# Predict the `medv` (median house value)
# View(Boston)
names(Boston)
? Boston

# Fit `medv` using lstat` (lower status of the population)
lm.fit = lm(medv ~ lstat, data = Boston)
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
        data.frame(lstat = c(5, 10, 15)),
        interval = "confidence") # CI
predict(lm.fit,
        data.frame(lstat = c(5, 10, 15)),
        interval = "prediction") # PI

# Plot
attach(Boston)
plot(lstat, medv, col = "red", pch = "+")
abline(lm.fit) # Draws the least squares regression line

# Diagnostic plots
par(mfrow = c(2, 2)) # Split the displayinto separate panels
plot(lm.fit) # Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage

# Residuals
par(mfrow = c(1, 1)) # Split the displayinto separate panels
plot(predict(lm.fit), residuals(lm.fit)) # Manually plot Residuals vs Fitted
plot(predict(lm.fit), rstudent(lm.fit)) # Studentized Residuals vs Fitted

# Leverage statistics
plot(hatvalues(lm.fit))

# Index of the largest element of a vector
which.max(hatvalues(lm.fit))

### 3.6.3 - Multiple Linear Regression
lm.fit = lm(medv ~ lstat + age, data = Boston) # `lstat` & `age`
summary(lm.fit)

lm.fit = lm(medv ~ ., data = Boston) # All predictors
summary(lm.fit)

# Statistics
summary(lm.fit)$r.sq # R-squared
summary(lm.fit)$sigma # RSE

# Variance Inflaction Factor
# install.packages("car")
library(car)
vif(lm.fit)

lm.fit = lm(medv ~ . - age, data = Boston) # All predictors except for `age`
summary(lm.fit)

lm.fit1 = update(lm.fit, ~ . - age) # Update function
summary(lm.fit1)

### 3.6.4 - Interation Terms
lm.fit = lm(medv ~ lstat + age + lstat:age, data = Boston)
lm.fit = lm(medv ~ lstat * age, data = Boston) # Shorthand
summary(lm.fit)

### 3.6.5 - Non-linear Transformations
lm.fit = lm(medv ~ lstat, data = Boston)
lm.fit2 = lm(medv ~ lstat + I(lstat ^ 2), data = Boston)
summary(lm.fit2)

# ANOVA to compare the two
# H0: both models fit equally well; Ha: full model (i.e. lm.fit2) is superior
anova(lm.fit, lm.fit2)
# Since F-statistics is 135.2 and p-value (Pr(>F)) is small, reject H0

par(mfrow = c(2, 2))
plot(lm.fit2)

# Polynomial
lm.fit5 = lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit5)
anova(lm.fit2, lm.fit5)
plot(lm.fit5)

lm.fit6 = lm(medv ~ poly(lstat, 6), data = Boston)
summary(lm.fit5)
anova(lm.fit5, lm.fit6) # Large p-value, adding 6th degree doesn't add value

lm.fit = lm(medv ~ poly(lstat, 5) + log(rm), data = Boston) # Log transformation
summary(lm.fit)
anova(lm.fit5, lm.fit)
plot(lm.fit)

### 3.6.6 - Qualitative Predictors

# View(Carseats)
names(Carseats)
? Carseats # `ShelveLoc` is qualitative

# R generates dummary variables automatically
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# Coding
attach(Carseats)
contrasts(ShelveLoc)

### 3.6.7 - Writing Functions

# Create a function that loads the necessary libraries
LoadLibraries = function() {
  library(ISLR)
  library(MASS)
  library(car)
  print("The libraries have been loaded.")
}

LoadLibraries # What's in the function, doesn't actually run it

LoadLibraries() # Runs