#### Misspecification of the Functional Form of the Regression Function #### 
library(AER) 
library(mvtnorm) 
library(stargazer)

# 1. Functional Form Misspecification
set.seed(3)

# (1) Create data set
X <- runif(100, -5, 5)   # n=100, between -5 to 5 
Y <- X^2 + rnorm(100)

# Estimate regression function
ms_mod <- lm(Y ~ X) 
ms_mod

# (2) Create plot 
# plot the data
plot(X, Y,
     main = "Misspecification of Functional Form", pch = 20,    # data is quadratic form
     col = "steelblue")

# plot the linear regression line
abline(ms_mod,
       col = "darkred",
       lwd = 2)

# 2. Functional Form Misspecification
set.seed(1)
library(mvtnorm)    # to simulate bivariate normal data

# (1) Create data set 
dat <- data.frame(
  rmvnorm(1000, c(50, 100),
          sigma = cbind(c(10, 5), c(5, 10))))

# (2) Rename column names 
colnames(dat) <- c("X","Y")

# (3) Estimate the model (without measurement error)
noerror_mod <- lm(Y ~ X, data = dat)

# (4) Estimate the model (with measurement error in X)
dat$X <- dat$X + rnorm(n = 1000, sd = sqrt(10)) 
error_mod <- lm(Y ~ X, data = dat)

# Extract coefficients
noerror_mod$coefficients

error_mod$coefficients

# (5) Plot the data 
plot(dat$X, dat$Y, pch = 20,
     col = "steelblue",
     xlab = "X",
     ylab = "Y")

abline(coef = c(75, 0.5), col = "yellow",    # no error coefficients 
       lwd = 1.5)
 
# Add regression line: no error
abline(noerror_mod, col = "purple",
       lwd = 1.5)

# Add regression line: no error
abline(error_mod,
       col = "green",
       lwd = 1.5)

# Legend
legend("topleft",
       bg = "transparent",
       cex = 0.8,
       lty = 1,
       col = c("darkgreen", "purple", "darkred"), 
       legend = c("Population", "No Errors", "Errors"))
####----------------------------------------------------------------------------------

#### Simultaneous causality (Y~X, and X~Y where X correlated with error) 
library(AER)
data("CigarettesSW")

c1995 <- subset(CigarettesSW, year == "1995")    # take subset for year 1995

cigcon_mod <- lm(log(packs) ~ log(price), data = c1995) 
cigcon_mod

# Scatter Plot
plot(log(c1995$price), log(c1995$packs), 
     xlab = "ln(Price)", ylab = "ln(Consumption)",
     main = "Demand for Cigarettes",
     pch = 20,
     col = "steelblue")

abline(cigcon_mod,
       col = "darkred",
       lwd = 1.5)
####----------------------------------------------------------------------------------

#### External Validity of the Study #### 
# 1. Analyze data  
# (1) Data Summary
data(MASchools) 
summary(MASchools)

# (2)Replicate Data Summary (Examine data)
MASchools$score <- MASchools$score4 
MASchools$STR <- MASchools$stratio

vars <- c("score", "STR", "english", "lunch", "income")
cbind(CA_mean = sapply(CASchools[,vars],mean),
      CA_sd = sapply(CASchools[,vars],sd),
      MA_mean = sapply(MASchools[,vars],mean),
      MA_sd = sapply(MASchools[,vars],sd))        # should give a summary table 

# 2. Three different Models (MA data) 
# Model 1: linear-linear
Linear_model_MA <- lm(score ~ income, data = MASchools) 
Linear_model_MA

# Model 2: linear-log 
Linearlog_model_MA <- lm(score ~ log(income), data = MASchools) 
Linearlog_model_MA

# Model 3: cubic 
cubic_model_MA <- lm(score ~ I(income) + I(income^2) + I(income^3), data = MASchools) 
cubic_model_MA

# Plot the results 
plot(MASchools$income, MASchools$score, pch = 20,
     col = "steelblue",
     xlab = "District income", ylab = "Test score", 
     xlim = c(0, 50), ylim = c(620, 780))

# Add regression line for Model 1
abline(Linear_model_MA, lwd = 2)

# Add regression line for Model 2
order_id <- order(MASchools$income)
lines(MASchools$income[order_id], 
      fitted(Linearlog_model_MA)[order_id], 
      col = "darkgreen",
      lwd = 2)

# Add regression line for Model 3
lines(x = MASchools$income[order_id],
      y = fitted(cubic_model_MA)[order_id], 
      col = "orange",
      lwd = 2)

legend("topleft",
       legend = c("Linear", "Linear-Log", "Cubic"), 
       lty = 1,
       col = c("Black", "darkgreen", "orange"))

# 3. Test model misspecifications
# (1) Create variable "HiEL"
MASchools$HiEL <- as.numeric(MASchools$english > median(MASchools$english))

# (2) Six different models 
TestScore_MA_mod1 <- lm(score ~ STR, data = MASchools) 
TestScore_MA_mod2 <- lm(score ~ STR + english + lunch + log(income), data = MASchools)
TestScore_MA_mod3 <- lm(score ~ STR + english + lunch + income + I(income^2) + I(income^3), data = MASchools)
TestScore_MA_mod4 <- lm(score ~ STR + I(STR^2) + I(STR^3) + english + lunch + income
                        + I(income^2) + I(income^3), data = MASchools) 
TestScore_MA_mod5 <- lm(score ~ STR + I(income^2) + I(income^3) + HiEL:STR + lunch + income, data = MASchools)
TestScore_MA_mod6 <- lm(score ~ STR + I(income^2) + I(income^3) + HiEL + HiEL:STR + lunch + income, data = MASchools)

rob_se <- list(sqrt(diag(vcovHC(TestScore_MA_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(TestScore_MA_mod2, type = "HC1"))), 
               sqrt(diag(vcovHC(TestScore_MA_mod3, type = "HC1"))), 
               sqrt(diag(vcovHC(TestScore_MA_mod4, type = "HC1"))), 
               sqrt(diag(vcovHC(TestScore_MA_mod5, type = "HC1"))), 
               sqrt(diag(vcovHC(TestScore_MA_mod6, type = "HC1"))))

library(stargazer)

stargazer(Linear_model_MA, TestScore_MA_mod2, TestScore_MA_mod3, TestScore_MA_mod4, 
          TestScore_MA_mod5, TestScore_MA_mod6, 
          title = "Regressions Using Massachusetts Test Score Data", 
          type = "text",
          digits = 3,
          header = FALSE,
          se = rob_se,
          object.names = TRUE,
          model.numbers = FALSE,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)"))

# (3) F-test for Model 3
linearHypothesis(TestScore_MA_mod3, c("I(income^2)=0", "I(income^3)=0"),
                 vcov. = vcovHC, type = "HC1")

# (4) F-test for Model 4 
linearHypothesis(TestScore_MA_mod4,
                 c("STR=0", "I(STR^2)=0", "I(STR^3)=0"),
                 vcov. = vcovHC, type = "HC1")

linearHypothesis(TestScore_MA_mod4, c("I(STR^2)=0", "I(STR^3)=0"),
                 vcov. = vcovHC, type = "HC1")

linearHypothesis(TestScore_MA_mod4, c("I(income^2)=0", "I(income^3)=0"),
                 vcov. = vcovHC, type = "HC1")

# (5) F-test for Model 5
linearHypothesis(TestScore_MA_mod5, c("STR=0", "STR:HiEL=0"),
                 vcov. = vcovHC, type = "HC1")

linearHypothesis(TestScore_MA_mod5, c("I(income^2)=0", "I(income^3)=0"),
                 vcov. = vcovHC, type = "HC1")

linearHypothesis(TestScore_MA_mod5, c("HiEL=0", "STR:HiEL=0"),
                 vcov. = vcovHC, type = "HC1")

# (6) F-test for Model 6
linearHypothesis(TestScore_MA_mod6, c("I(income^2)=0", "I(income^3)=0"),
                 vcov. = vcovHC, type = "HC1")

TestScore_MA_mod3$coefficients[2] / sd(MASchools$score) * (-2)

TestScore_mod2$coefficients[2] / sd(CASchools$score) * (-2)
####----------------------------------------------------------------------------------