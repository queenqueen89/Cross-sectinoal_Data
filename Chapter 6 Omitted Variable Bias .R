#### OVB #### 
library(AER)
data(CASchools)

CASchools$STR <- CASchools$students/CASchools$teachers 
CASchools$score <- (CASchools$read + CASchools$math)/2

cor(CASchools$STR, CASchools$score)
cor(CASchools$STR, CASchools$english)

# estimate both regression models
mod <- lm(score ~ STR, data = CASchools)
mult.mod <- lm(score ~ STR + english, data = CASchools)

mod$coefficients
mult.mod$coefficients
####----------------------------------------------------------------------------------

#### Measures of Fit in Multiple Regression #### 
summary(mult.mod)

# calculate R2 by hand 
# define the components
n <- nrow(CASchools)    # number of observations (rows)
k<-2         # number of regressors
y_mean <- mean(CASchools$score)   # mean of avg. test-scores
SSR <- sum(residuals(mult.mod)^2)
TSS <- sum((CASchools$score - y_mean )^2) 
ESS <- sum((fitted(mult.mod) - y_mean)^2)

# compute the measures
SER <- sqrt(1/(n-k-1) * SSR)    # standard error of the regression
Rsq <- 1 - (SSR / TSS)    # R^2
adj_Rsq <- 1 - (n-1)/(n-k-1) * SSR/TSS   # adj. R^2

# print the measures to the console
c("SER" = SER, "R2" = Rsq, "Adj.R2" = adj_Rsq)
####----------------------------------------------------------------------------------

#### OLS Assumptions in Multiple Regression #### 
# define the fraction of English learners
CASchools$FracEL <- CASchools$english / 100     # estimate the model
mult.mod <- lm(score ~ STR + english + FracEL, data = CASchools)

# obtain a summary of the model
summary(mult.mod)

# if STR smaller 12, NS = 0, else NS = 1
CASchools$NS <- ifelse(CASchools$STR < 12, 0, 1)    # estimate the model
mult.mod <- lm(score ~ computer + english + NS, data = CASchools)

# obtain a model summary
summary(mult.mod)

table(CASchools$NS)

# set seed for reproducibility
set.seed(1)

# generate artificial data on location
CASchools$direction <- sample(c("West", "North", "South", "East"), 420, replace = T)
mult.mod <- lm(score ~ STR + english + direction, data = CASchools)

# obtain a model summary
summary(mult.mod)

# Percentage of english speakers
CASchools$PctES <- 100 - CASchools$english     # estimate the model
mult.mod <- lm(score ~ STR + english + PctES, data = CASchools)

# obtain a model summary
summary(mult.mod)

# load packages
library(MASS) 
library(mvtnorm)

# set number of observations
n <- 50

# initialize vectors of coefficients
coefs1 <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000)) 
coefs2 <- coefs1

set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))   # for cov(X_1,X_2) = 0.25
  u <- rnorm(n, sd = 5)     
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs1[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 8.5), c(8.5, 10)))     # for cov(X_1,X_2) = 0.85
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u      
  coefs2[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
}

# obtain variance estimates
diag(var(coefs1))

diag(var(coefs2))
####----------------------------------------------------------------------------------

#### Distribution of the OLS Estimators in Multiple Regression #### 
# load packages
library(MASS) 
library(mvtnorm)

# set sample size
n <- 50

# initialize vector of coefficients
coefs <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))   # set seed for reproducibility
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10))) 
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs[i,] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
}

# compute density estimate
kde <- kde2d(coefs[, 1], coefs[, 2])

# plot density estimate
persp(kde,
      theta = 310,
      phi = 30,
      xlab = "beta_1",
      ylab = "beta_2",
      zlab = "Est. Density")

# estimate the correlation between estimators
cor(coefs[, 1], coefs[, 2]) 
####----------------------------------------------------------------------------------