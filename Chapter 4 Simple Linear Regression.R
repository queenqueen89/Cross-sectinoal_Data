#### Simple linear regression #### 
Testscore <- c(680, 640, 670, 660, 630, 660, 635)  
STR <- c(15, 17, 19, 20, 22, 23.5, 25)             
plot(Testscore ~ STR)
abline(a = 713, b = -3, col = 3)                    # where a=713 is intercept, b=-3 is slope
####----------------------------------------------------------------------------------

#### Estimate coefficients of the linear regression model ####
# (1) Distribution summary(average, SD, quantitle) for Testscore & Student-teacher ratio 
library(AER)
library(MASS)
data(CASchools)     

CASchools$STR <- CASchools$students/CASchools$teachers   # calculate student-teacher ratio and append it to CASchools
CASchools$score <- (CASchools$read + CASchools$math)/2   # calculate Testscore and append it to CASchools

avg_STR <- mean(CASchools$STR)       # find sample averages of STR
avg_score <- mean(CASchools$score)   # find sample averages of score

sd_STR <- sd(CASchools$STR)         # find sample sd of STR
sd_score <- sd(CASchools$score)     # find sample sd of score

quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)    # create a vector of percentiles 
quant_STR <- quantile(CASchools$STR, quantiles)         # find quantiles of STR
quant_score <- quantile(CASchools$score, quantiles)     # find quantiles of score

DistributionSummary <- data.frame(Average = c(avg_STR, avg_score),          # get distribution summary-averages for STR & score
                                  standardDeviation = c(sd_STR, sd_score),  # sd for STR & score
                                  quantitle = c(quant_STR, quant_score))    # quantitle for STR & score
DistributionSummary

plot(score ~ STR,            # scatterplot for score ~ STR
     data = CASchools,       # for data CASchools
     main = "Scatterplot of Testscore and STR",
     xlab = "STR(X)",
     ylab = "Test score(Y)")
cor(CASchools$STR, CASchools$score)

# (2) Ordinary least squares estimator 
attach(CASchools)    # attach() allow to use variables contained in CASchools direclty 

beta_1 <- sum((STR - mean(STR)) * (score - mean(score))) / sum((STR - mean(STR))^2)   # directly calculate beta_1
beta_0 <- mean(score) - beta_1 * mean(STR)    # directly calculate beta_0
beta_1
beta_0

linear_model <- lm(score ~ STR, data = CASchools)   # lm() to directly get linear regression
linear_model

range(score)    # use range() to find score & STR range
range(STR)

plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of Testscore and STR",
     xlab = "STR(X)",
     ylab = "score(Y)",
     xlim = c(10, 30),
     ylim = c(600, 720))
abline(linear_model)
####----------------------------------------------------------------------------------

#### Measures of fit (R^2) ####
model_summary <- summary(linear_model)   # get model summary from linear model   
model_summary

SSR <- sum(model_summary$residuals^2)    # find SSR 
TSS <- sum((score - mean(score))^2)    # find SST
print(R2 <- 1 - SSR/TSS)               # find R^2

n <- nrow(CASchools)                   # find number of rows for data CASchools
print(SER <- sqrt(SSR / (n-2)))        # find SER (standard error of regression)
####----------------------------------------------------------------------------------

#### Least squares assumptions ####
# (1) Assumption 1: the error term has zero conditional mean 
set.seed(321)

X <- runif(50, min = -5, max = 5)   # simulate the data for X and residual u
u <- rnorm(50, sd=5)

Y <- X^2 + 2*X + u       # suppose true model is this one

mod_simple <- lm(Y ~ X)  # run linear regression based on the simulated data

prediction <- predict(lm(Y ~ X + I(X^2)), data.frame(X = sort(X)))  # Predictions with quadratic model. 
                                                                    # I() in function formula: inhibit interpretation of operators ^ as formula operators, but used as arithmetical operators 
                                                                    # sort(): Sort/order a vector into ascending/descending order. 
plot(Y ~ X)                                # plot simple linear regression model 
abline(mod_simple, col = "red")
lines(sort(X), prediction, col = "blue")   # plot the quadratic model

# Note: sort vector 
x <- c(3,4,5,2,0,-1,3)    
sort(x, decreasing = T)    # sort in decreasing order
sort(x)                    # by default, sort in increasing order

# (2) Assumption 2: iid data
set.seed(123)

Date <- seq(as.Date("1951/1/1"), as.Date("2000/1/1"), "years")   # generate a date vector 

X <- c(5000, rep(NA, length(Date)-1))     # initialize the employment vector, starting year is 1950, so we have "length(Date)-1". The number of workers is 5000. 

for (i in 2:length(Date)) {
        X[i] <- -50 + 0.98 * X[i-1] + rnorm(n = 1, sd = 200)
}

plot(x = Date, 
     y = X, 
     type = "l",
     col = "deeppink",
     ylab = "Workers",
     xlab = "Time")

# (3) Assumption 3: large outliers are unlikely
set.seed(123)

X <- sort(runif(10, min = 30, max = 70))

Y <- rnorm(10, mean = 200, sd = 50)
Y[9] <- 2000

fit <- lm(Y ~ X)

fit_NoOutlier <- lm(Y[-9] ~ X[-9])

plot(Y ~ X)
abline(fit, col = "blue")
abline(fit_NoOutlier, col = "deeppink2")
legend("topleft",
       legend = c("Regression with Outlier","Regression without Outlier "),
       lty = c(1,1),
       col = c("blue","deeppink2"),
       lwd = 2)
####----------------------------------------------------------------------------------

#### Sampling distribution of OLS estimator #### 
# (1) Simulation study 1
N <- 100000
X <- runif(N, min = 0, max = 20)
u <- rnorm(N, sd = 10)

Y <- -2 + 3.5*X + u
population <- data.frame(X,Y)

n <- 100

H_i <- 1 - mean(X) / mean(X^2) * X
var_b0 <- var(H_i * u) / (n * mean(H_i^2)^2)

var_b1 <- var((X - mean(X)) * u) / (100 * var(X)^2)

var_b0
var_b1

n <- 100      # generate samole size
reps <- 10000   # generate repetitions

fit <- matrix(ncol = 2, nrow = reps)    # initialize the matrix of outcomes 

for (i in 1:reps) {          # loop sampling and estimation of coefficients 
        sample <- population[sample(1:N, n), ]
        fit[i, ] <- lm(Y ~ X, data = sample)$coefficients
}

var(fit[,1])    # find variance using 1st outcome
var(fit[,2])    # find variance using 2nd outcome

par(mfrow = c(1,2))  # divide plot area as 1-by-2 array

hist(fit[,1],       # plot histogram for the distribution of 10000 beta[0] estimates
     cex.main = 1,  #
     main = bquote(The~Distribution~of~10000~beta[0]~Estimates),
     xlab = bquote(hat(beta)[0]),
     freq = F)

curve(dnorm(x,
            -2,
            sqrt(var_b0)),
      add = T,
      col = "deeppink2")

hist(fit[,2],      # plot histogram for the distribution of 10000 beta[1] estimates
     cex.main = 1,
     main = bquote(The~Distribution~of~10000~beta[1]~Estimates),
     xlab = bquote(hat(beta)[1]),
     freq = F)

curve(dnorm(x,
            3.5,
            sqrt(var_b1)),
      add = T,
      col = "gold")

# (2) Simulation study 2
set.seed(1)

reps <- 1000        # repititions 
n <- c(100, 250, 1000, 3000)   # a vector of sample sizes 

fit <- matrix(ncol = 2, nrow = reps)   # initialize the matrix of outcomes 

par(mfrow = c(2,2))    

for (j in 1:length(n)) {         # outer loop over n
        for (i in 1:reps) {      # inner loop: sampling & estimating of the coefficient 
                sample <- population[sample(1:N, n[j]),]
                fit[i,] <- lm(Y ~ X, data = sample)$coefficients
        }
        plot(density(fit[,2]), xlim = c(2.5, 4.5),    # draw density estimates 
             col = j,
             main = paste("n=", n[j]),       # use paste() to use the title j times 
             xlab = bquote(hat[beta][1]))    # use bquote() to include strings, math expr, numbers, variables.  
}

# (3) Simulation study 3
set.seed(4)

bvndata <- mvrnorm(100,           # simulate bivariate normal data
                   mu = c(5,5),
                   Sigma = cbind(c(5,4), c(4,5)))

colnames(bvndata) <- c("X", "Y")      # assign col names
bvndata <- as.data.frame(bvndata)     # covert to data.frame

set1 <- subset(bvndata, abs(mean(X) - X) > 1)    # subset the data
set2 <- subset(bvndata, abs(mean(X) - X) <= 1)

plot(set1,
     xlab = "X",
     ylab = "Y",
     pch = 19,
     col = "deeppink2")      # pch=19: the points are filled circles 

points(set2,
       col = "gold",
       pch = 19)

lm.set1 <- lm(Y ~ X, data = set1)
lm.set2 <- lm(Y ~ X, data = set2)

plot(set1, xlab = "X", ylab = "Y", pch = 19)   # plot set1
points(set2, col = "red", pch = 19)           # use points() to add set2 in the same plot of set1. If use plot(), it'll make a separate plot.

abline(lm.set1, col = "black")
abline(lm.set2, col = "red")
####----------------------------------------------------------------------------------