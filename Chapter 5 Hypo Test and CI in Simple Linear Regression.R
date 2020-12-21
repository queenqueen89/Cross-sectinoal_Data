#### 2-Sided Hypo Test #### 
library(AER) 
library(scales)
data(CASchools)

# 1. Calculate p-value 
CASchools$STR <- CASchools$students/CASchools$teachers    # student-teacher ratio
CASchools$score <- (CASchools$read + CASchools$math)/2    # average test-score

linear_model <- lm(score ~ STR, data = CASchools)
summary(linear_model)
summary(linear_model)$coefficients
linear_model$df.residual      # determine residual degrees of freedom

2 * pt(-4.751327, df = 418)   # p-value for 2-sided test 

2 * pnorm(-4.751327)    # use the standard normal density to compute the p-value

# 2. Plot the standard normal on the support [-6,6]
t <- seq(-6, 6, 0.01)
plot(x = t,
     y = dnorm(t, 0, 1),
     type = "l",
     col = "steelblue",
     lwd = 2,
     yaxs = "i",
     axes = F,
     ylab = "",
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=-0.47"), 
     cex.lab = 0.7,
     cex.main = 1)
     
tact <- -4.75

axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7) # Shade the critical regions using polygon():

# 3. critical region in left tail
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96), y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0), col = 'orange')

# 4. critical region in right tail
polygon(x = c(1.96, seq(1.96, 6, 0.01), 6), y = c(0, dnorm(seq(1.96, 6, 0.01)), 0), col="orange")

# 5. Add arrows and texts indicating critical regions and the p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1) 
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)
arrows(-5, 0.16, -4.75, 0, length = 0.1) 
arrows(5, 0.16, 4.75, 0, length = 0.1)
text(-3.5, 0.22, labels = expression("0.025"~"="~over(alpha, 2)), cex = 0.7)
text(3.5, 0.22,labels = expression("0.025"~"="~over(alpha, 2)), cex = 0.7)
text(-5, 0.18,labels = expression(paste("-|",t[act],"|")), cex = 0.7)
text(5, 0.18,labels = expression(paste("|",t[act],"|")), cex = 0.7)

# 6. Add ticks indicating critical values at the 0.05-level, t^act and -t^act
rug(c(-1.96, 1.96), ticksize = 0.145, lwd = 2, col = "darkred") 
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")
####----------------------------------------------------------------------------------

#### CI for coefficients #### 
# (1) Simulation: CI
set.seed(4)

# generate and plot the sample data
Y <- rnorm(n = 100, mean = 5,
           sd = 5)
plot(Y,
     pch = 19,
     col = "steelblue")

cbind(CIlower = mean(Y) - 1.96 * 5 / 10, CIupper = mean(Y) + 1.96 * 5 / 10)

# (2) Simulation: CI
set.seed(1)

# initialize vectors of lower and upper interval boundaries
lower <- numeric(10000) 
upper <- numeric(10000)

# loop sampling / estimation / CI
for(i in 1:10000) {
  Y <- rnorm(100, mean = 5, sd = 5) 
  lower[i] <- mean(Y) - 1.96 * 5 / 10 
  upper[i] <- mean(Y) + 1.96 * 5 / 10
}

# join vectors of interval bounds in a matrix
CIs <- cbind(lower, upper)

mean(CIs[, 1] <= 5 & 5 <= CIs[, 2])

# (3) identify intervals not covering mu
# (4 intervals out of 100)
ID <- which(!(CIs[1:100, 1] <= 5 & 5 <= CIs[1:100, 2]))
# initialize the plot
plot(0,
     xlim = c(3, 7),
     ylim = c(1, 100),
     ylab = "Sample",
     xlab = expression(mu),
     main = "Confidence Intervals")

# set up color vector
colors <- rep(gray(0.6), 100) colors[ID] <- "red"

# draw reference line at mu=5
abline(v = 5, lty = 2)

# add horizontal bars representing the CIs
for(j in 1:100) { 
  lines(c(CIs[j, 1], CIs[j, 2]),
        c(j, j),
        col = colors[j], lwd = 2)
}

# compute 95% CI
confint(linear_model)     

# compute 95% CI by hand 
lm_summ <- summary(linear_model)
c("lower" = lm_summ$coef[2,1] - qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2], 
  "upper" = lm_summ$coef[2,1] + qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2])
####----------------------------------------------------------------------------------

#### Regression: X is binary #### 
# Create the dummy variable as defined above
CASchools$D <- CASchools$STR < 20

# Plot the data
plot(CASchools$D, CASchools$score, pch = 20,
     cex = 0.5,
     col = "Steelblue",
     xlab = expression(D[i]), ylab = "Test Score",
     main = "Dummy Regression")
# provide the data to be plotted
# use filled circles as plot symbols
# set size of plot symbols to 0.5
# set the symbols' color to "Steelblue"
# Set title and axis names

# estimate the dummy regression model
dummy_model <- lm(score ~ D, data = CASchools) 
summary(dummy_model)

# add group specific predictions to the plot
points(x = CASchools$D,
       y = predict(dummy_model),
       col = "red",
       pch = 20)

confint(dummy_model)
####----------------------------------------------------------------------------------

#### Heteroskedasticity and Homoskedasticity #### 
library(scales)
set.seed(123)

# 1. generate some heteroskedastic data:
# set up vector of x coordinates
x <- rep(c(10, 15, 20, 25), each = 25) # initialize vector of errors
e <- c()

# sample 100 errors such that the variance increases with x
e[1:25] <- rnorm(25, sd = 10) 
e[26:50] <- rnorm(25, sd = 15) 
e[51:75] <- rnorm(25, sd = 20) 
e[76:100] <- rnorm(25, sd = 25)

# set up y
y <- 720 - 3.3 * x + e # Estimate the model
mod <- lm(y ~ x)

# Plot the data
plot(x = x, y = y,
     main = "An Example of Heteroskedasticity",
     xlab = "Student-Teacher Ratio",
     ylab = "Test Score",
     cex = 0.5,
     pch = 19,
     xlim = c(8, 27), ylim = c(600, 710))

# Add the regression line to the plot
abline(mod, col = "darkred")

# Add boxplots to the plot
boxplot(formula = y ~ x, add = TRUE,
        at = c(10, 15, 20, 25), col = alpha("gray", 0.4), border = "black")

# Application 
library(AER) 
data("CPSSWEducation") 
attach(CPSSWEducation)

# get an overview
summary(CPSSWEducation)

labor_model <- lm(earnings ~ education)

plot(earnings ~ education,
     ylim = c(0, 150))

abline(labor_model,
       col = "steelblue",
       lwd = 2)

confint(labor_model)

# Store model summary in 'model'
model <- summary(linear_model)

# Extract the standard error of the regression from model summary
SER <- model$sigma

# Compute the variation in 'size'
V <- (nrow(CASchools)-1) * var(CASchools$STR)

# Compute the standard error of the slope parameter's estimator and print it
SE.beta_1.hat <- sqrt(SER^2/V) 
SE.beta_1.hat

# Use logical operators to see if the value computed by hand matches the one provided 
# in mod$coefficients. Round estimates to four decimal places
round(model$coefficients[2, 2], 4) == round(SE.beta_1.hat, 4)   # check if two sides are equal 

# compute heteroskedasticity-robust standard errors
vcov <- vcovHC(linear_model, type = "HC1") 
vcov

# compute the square root of the diagonal elements in vcov
robust_se <- sqrt(diag(vcov)) 
robust_se

# we invoke the function `coeftest()` on our model
coeftest(linear_model, vcov. = vcov)

# generate heteroskedastic data
X <- 1:500
Y <- rnorm(n = 500, mean = X, sd = 0.6 * X)

# estimate a simple regression model
reg <- lm(Y ~ X)

# plot the data
plot(x = X, y = Y, pch = 19,
     col = "steelblue",
     cex = 0.8)

# add the regression line to the plot
abline(reg,
       col = "darkred",
       lwd = 1.5)

# test hypothesis using the default standard error formula
linearHypothesis(reg, hypothesis.matrix = "X = 1")$'Pr(>F)'[2] < 0.05

# test hypothesis using the robust standard error formula
linearHypothesis(reg, hypothesis.matrix = "X = 1", white.adjust = "hc1")$'Pr(>F)'[2] < 0.05

# initialize vectors t and t.rob
t <- c() 
t.rob <- c()

# loop sampling and estimation
for (i in 1:10000) {         
  X <- 1:1000            # sample data
  Y <- rnorm(n = 1000, mean = X, sd = 0.6 * X)
  reg <- lm(Y ~ X)       # estimate regression model
  t[i] <- linearHypothesis(reg, "X = 1")$'Pr(>F)'[2] < 0.05 # robust significance test 
  t.rob[i] <- linearHypothesis(reg, "X = 1", white.adjust = "hc1")$'Pr(>F)'[2] < 0.05 }

# compute the fraction of false rejections
round(cbind(t = mean(t), t.rob = mean(t.rob)), 3)
####----------------------------------------------------------------------------------

#### Gauss-Markov Theorem #### 
# set sample size and number of repetitions
n <- 100
reps <- 1e5

# choose epsilon and create a vector of weights as defined above
epsilon <- 0.8
w <- c(rep((1 + epsilon) / n, n / 2),
       rep((1 - epsilon) / n, n / 2) )

# draw a random sample y_1,...,y_n from the standard normal distribution,
# use both estimators 1e5 times and store the result in the vectors 'ols' and 'weightedestimator'
ols <- rep(NA, reps) 
weightedestimator <- rep(NA, reps)
for (i in 1:reps) {
  y <- rnorm(n)
  ols[i] <- mean(y)
  weightedestimator[i] <- crossprod(w, y)
}

# plot kernel density estimates of the estimators' distributions:

# OLS
plot(density(ols), col = "purple",
     lwd = 3,
     main = "Density of OLS and Weighted Estimator",
     xlab = "Estimates")

# weighted
lines(density(weightedestimator), col = "steelblue",
      lwd = 3)

# add a dashed line at 0 and add a legend to the plot
abline(v = 0, lty = 2)
legend('topright',
       c("OLS", "Weighted"),
       col = c("purple", "steelblue"), lwd = 3)
####----------------------------------------------------------------------------------

#### Use t-stat when sample small #### 
# initialize two vectors
beta_0 <- c() beta_1 <- c()
# loop sampling / estimation / t statistics
for (i in 1:10000) {
  X <- runif(20, 0, 20)
  Y <- rnorm(n = 20, mean = X)
  reg <- summary(lm(Y ~ X))
  beta_0[i] <- (reg$coefficients[1, 1] - 0)/(reg$coefficients[1, 2]) beta_1[i] <- (reg$coefficients[2, 1] - 1)/(reg$coefficients[2, 2])
}
# plot the distributions and compare with t_18 density:
# divide plotting area
par(mfrow = c(1, 2))
# plot the simulated density of beta_0
plot(density(beta_0), lwd = 2 ,
     main = expression(widehat(beta)[0]), xlim = c(-4, 4))
# add the t_18 density to the plot
curve(dt(x, df = 18), add = T,
      col = "red",
      lwd = 2,
      lty = 2)
# plot the simulated density of beta_1
plot(density(beta_1), lwd = 2,
     main = expression(widehat(beta)[1]), xlim = c(-4, 4) )
# add the t_18 density to the plot
curve(dt(x, df = 18), add = T,
      col = "red",
      lwd = 2,
      lty = 2)
####----------------------------------------------------------------------------------