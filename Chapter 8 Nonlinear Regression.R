#### General Strategy for modeling Nonlinear Regression Functions ####
library(AER) 
library(stargazer)
data(CASchools)
CASchools$size <- CASchools$students/CASchools$teachers 
CASchools$score <- (CASchools$read + CASchools$math) / 2

# 1. Income and Score Correlation
cor(CASchools$income, CASchools$score)

linear_model<- lm(score ~ income, data = CASchools)

# 2. Scatter plot for income and score
# (1) Method 1 scatter plot 
plot(CASchools$income, CASchools$score,
     col = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     cex.main = 0.9,
     main = "Test Score vs. District Income and a Linear OLS Regression Function")

# (2) Method 2 scatter plot 
plot(score ~ income, data = CASchools, 
     col = "steelblue", 
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     cex.main = 0.9,
     main = "Test Score vs. District Income and a Linear OLS Regression Function")

# 3. Add regression line 
abline(linear_model, col = "black", lwd = 2)

# 4. Quadratic model 
quadratic_model <- lm(score ~ income + I(income^2), data = CASchools)  # use I() function transform income2 

coeftest(quadratic_model, vcov. = vcovHC, type = "HC1")

# 4. Add quadratic function to the plot
order_id <- order(CASchools$income)      # order() ranks income from low to high by default
lines(x = CASchools$income[order_id],
      y = fitted(quadratic_model)[order_id], col = "red",
      lwd = 2)
####----------------------------------------------------------------------------------

#### Nonlinear functions of single independent variable #### 
# 1. Cubic Model 
cubic_model <- lm(score ~ poly(income, degree = 3, raw = TRUE), data = CASchools)   # income in polynomial 3 

summary(cubic_model)

# 2. F-Test 
R <- rbind(c(0, 0, 1, 0), c(0, 0, 0, 1))     # test H0: beta2=beta3 = 0
linearHypothesis(cubic_model, 
                 hypothesis.matrix = R,
                 white.adj = "hc1")

# 3. Test coefficients significance Robust 
coeftest(cubic_model, vcov. = vcovHC, type = "HC1")

# 4. Robust F-test 
linearHypothesis(cubic_model, 
                 hypothesis.matrix = R,
                 vcov. = vcovHC, type = "HC1")
####----------------------------------------------------------------------------------

#### Interpret Coefficients in Nonlinear Models#### 
# (1) Quadratic model 
quadriatic_model <- lm(score ~ income + I(income^2), data = CASchools)
summary(CASchools$income)

# (2) New data 
new_data <- data.frame(income = c(10, 11))

# (3) Prediction with new data 
Y_hat <- predict(quadriatic_model, newdata = new_data)   

# (4) Take difference
diff(Y_hat) # predict score difference for income from 10 to 11 

#### Interpret Coefficients in Nonlinear Models #### 
quadriatic_model <- lm(score ~ income + I(income^2), data = CASchools)

new_data2 <- data.frame(income = c(40, 41))
Y_hat2 <- predict(quadriatic_model, newdata = new_data2)   

diff(Y_hat2) # predict score difference for income from 40 to 41 
####----------------------------------------------------------------------------------

#### Logarithms #### 
# 1. log(x)
LinearLog_model <- lm(score ~ log(income), data = CASchools)

coeftest(LinearLog_model, 
         vcov. = vcovHC, type="HC1")

plot(score ~ income, col = "steelblue",
     pch = 20,
     data = CASchools,
     main = "Linear-Log Regression Line")

order_id <- order(CASchools$income)
lines(x = CASchools$income[order_id],
      y = fitted(LinearLog_model)[order_id], col = "red",
      lwd = 2)

# (1) new data
new_data <- data.frame(income = c(10, 11, 40, 41))

# (2) predict scores 
Y_hat <- predict(LinearLog_model, newdata = new_data)

# (3) differences in scores 
Y_hat_matrix <- matrix(Y_hat, nrow = 2, byrow = TRUE) 
Y_hat_matrix[, 2] - Y_hat_matrix[, 1]

# 2. log(y)
LogLinear_model <- lm(log(score) ~ income, data = CASchools)

coeftest(LogLinear_model,
         vcov = vcovHC, type = "HC1")

# (1) scatter plot 
plot(log(score) ~ income, data = CASchools,
     col = "steelblue",
     pch = 20,
     main = "Log-Linear Regression Function")

# (2) add log-linear regression line
order_id <- order(CASchools$income)
lines(CASchools$income[order_id], 
      fitted(LogLinear_model)[order_id], 
      col = "red",
      lwd = 2)

# (3) add log-log regression line
lines(sort(CASchools$income), 
      fitted(LogLog_model)[order(CASchools$income)], 
      col = "green",
      lwd = 2)

legend("bottomright",
      legend = c("log-linear model", "log-log model"), lwd = 2,
      col = c("red", "green"))

# 3. log(x) and log(y)
LogLog_model <- lm(log(score) ~ log(income), data = CASchools)

coeftest(LogLog_model,
         vcov = vcovHC, type = "HC1")

# 4. Polylog Model 
polyLog_model <- lm(score ~ log(income) + I(log(income)^2) + I(log(income)^3), 
                    data = CASchools)

coeftest(polyLog_model,
         vcov = vcovHC, type = "HC1")

# (1) Adjusted R2
adj_R2 <-rbind("Quadratic" = summary(quadratic_model)$adj.r.squared, 
               "Cubic" = summary(cubic_model)$adj.r.squared,
               "LinearLog" = summary(LinearLog_model)$adj.r.squared, 
               "LogLinear" = summary(LogLinear_model)$adj.r.squared, 
               "LogLog" = summary(LogLog_model)$adj.r.squared, 
               "PolyLog" = summary(polyLog_model)$adj.r.squared)

colnames(adj_R2) <- "adj_R2"

adj_R2

# (2) Scatter plot 
# Scatter plot
plot(score ~ income, data = CASchools,
     col = "steelblue",
     pch = 20,
     main = "Linear-Log and Cubic Regression Functions")

# linear-log regression line  
order_id <- order(CASchools$income)
lines(CASchools$income[order_id], 
      fitted(LinearLog_model)[order_id], 
      col = "darkgreen",
      lwd = 2)
# cubic regression line
lines(x = CASchools$income[order_id],
      y = fitted(cubic_model)[order_id], col = "darkred",
      lwd = 2)
####----------------------------------------------------------------------------------

#### Interaction between 2 binary variables #### 
# 1.Dummies "HiSTR" and "HiEL" interactions
CASchools$HiSTR <- as.numeric(CASchools$size >= 20)    # transform to dummy variable 
CASchools$HiEL <- as.numeric(CASchools$english >= 10)

bi_model <- lm(score ~ HiSTR*HiEL, data=CASchools) # gives HiSTR, HiEL, and HiSTR*HiEL
coeftest(bi_model, vcov.=vcovHC, type="HC1")

# (1) Predict values for different HiSTR and HiEL
predict(bi_model, newdata = data.frame("HiSTR" = 0, "HiEL" = 0))
predict(bi_model, newdata = data.frame("HiSTR" = 0, "HiEL" = 1))
predict(bi_model, newdata = data.frame("HiSTR" = 1, "HiEL" = 0))
predict(bi_model, newdata = data.frame("HiSTR" = 1, "HiEL" = 1))

# (2) artificial data 
set.seed(1)

X <- runif(200, 0, 15)    # n=200, min=0, max=15
D <- sample(0:1, 200, replace = T)    # n=200, pick 0 or 1 randomly  # this is binary variable  

Y <- 450 + 150*X + 500*D + 50*(X*D) + rnorm(200,sd=300)

# (3) Compare different models 
m <- rbind(c(1, 2), c(3, 0)) 
graphics::layout(m)

# baseline model 
plot(X, log(Y), pch = 20,
     col = "steelblue",
     main = "Different Intercepts, Same Slope")

mod1_coef <- lm(log(Y) ~ X + D)$coefficients
abline(coef = c(mod1_coef[1], mod1_coef[2]), col = "red",
       lwd = 1.5)
abline(coef = c(mod1_coef[1] + mod1_coef[3], mod1_coef[2]), col = "green",
       lwd = 1.5)

# baseline model + interaction term 
plot(X, log(Y), pch = 20,
     col = "steelblue",
     main = "Different Intercepts, Different Slopes")

mod2_coef <- lm(log(Y) ~ X + D + X:D)$coefficients
abline(coef = c(mod2_coef[1], mod2_coef[2]), 
       col = "red",
       lwd = 1.5)
abline(coef = c(mod2_coef[1] + mod2_coef[3], mod2_coef[2] + mod2_coef[4]), 
       col = "green",
       lwd = 1.5)

# Omit regression D + interaction term
plot(X, log(Y), pch = 20,
     col = "steelblue",
     main = "Same Intercept, Different Slopes")

mod3_coef <- lm(log(Y) ~ X + X:D)$coefficients
abline(coef = c(mod3_coef[1], mod3_coef[2]), col = "red",
       lwd = 1.5)
abline(coef = c(mod3_coef[1], mod3_coef[2] + mod3_coef[3]), col = "green",
       lwd = 1.5)

# 2. "size" and "HiEL" as regressors
bci_model <- lm(score ~ size + HiEL + size * HiEL, data = CASchools)
coeftest(bci_model, vcov. = vcovHC, type = "HC1")

# Scatter plot 
id <- CASchools$english >= 10

# HiEL = 0 as red dots
plot(CASchools$size[!id], CASchools$score[!id],    # !id means not in id
     xlim = c(0, 27), ylim = c(600, 720), 
     pch = 20,
     col = "red",
     main = "",
     xlab = "Class Size",
     ylab = "Test Score")

# HiEL = 1 as green dots
points(CASchools$size[id], CASchools$score[id], 
       pch = 20,
       col = "green")

# Coefficients 
coefs <- bci_model$coefficients

# Regression line for HiEL = 0
abline(coef = c(coefs[1], coefs[2]), 
       col = "red",
       lwd = 1.5)

# Regression line for HiEL = 1
abline(coef = c(coefs[1] + coefs[3], coefs[2] + coefs[4]), 
       col = "green",
       lwd = 1.5 )

# Legend
legend("topright",
       pch = c(20, 20),
       col = c("red", "green"),
       legend = c("HiEL = 0", "HiEL = 1"))
####----------------------------------------------------------------------------------

#### Interactions Between Two Continuous Variables #### 

# 1.Regressors "English" and "size" interaction 
cci_model <- lm(score ~ size + english + english * size, data = CASchools)

coeftest(cci_model, vcov. = vcovHC, type = "HC1")

summary(CASchools$english)

# 2. Demand for Economic Journals 
library(AER) 
data("Journals")

# (1) Create new variables 
Journals$PricePerCitation <- Journals$price/Journals$citations 
Journals$Age <- 2000 - Journals$foundingyear 
Journals$Characters <- Journals$charpp * Journals$pages/10^6 
Journals$Subscriptions <- Journals$subs

# (2) Price per citation 
summary(Journals$PricePerCitation)     # range is very large 

# (3) Four different log-log models 
Journals_mod1 <- lm(log(Subscriptions) ~ log(PricePerCitation), data = Journals)

Journals_mod2 <- lm(log(Subscriptions) ~ log(PricePerCitation) + log(Age) + log(Characters), data = Journals)

# For quadratic & cubic terms:
# method 1 (use I() function)
Journals_mod3_1 <- lm(log(Subscriptions) ~ log(PricePerCitation) + I(log(PricePerCitation)^2) + 
                      I(log(PricePerCitation)^3) + log(Age) + log(Characters) + log(Age):log(PricePerCitation), 
                      data = Journals)

# method 2 (use poly() function)
Journals_mod3_2 <- lm(log(Subscriptions) ~ poly(log(PricePerCitation), degree = 3, raw = TRUE) + log(Age) 
                    + log(Characters) + log(Age):log(PricePerCitation), data = Journals)

Journals_mod4 <- lm(log(Subscriptions) ~ log(PricePerCitation) + log(Age)
                    + log(Age):log(PricePerCitation) + log(Characters), data = Journals)

linearHypothesis(Journals_mod3_1, 
                 c("I(log(PricePerCitation)^2)=0", "I(log(PricePerCitation)^3)=0"),
                 vcov. = vcovHC, type = "HC1")

# (4) Compare the 4 models 
library(stargazer)

rob_se <- list(sqrt(diag(vcovHC(Journals_mod1, type="HC1"))),
               sqrt(diag(vcovHC(Journals_mod2, type="HC1"))),
               sqrt(diag(vcovHC(Journals_mod3_1, type="HC1"))),
               sqrt(diag(vcovHC(Journals_mod4, type="HC1"))))

stargazer(Journals_mod1, Journals_mod2, Journals_mod3_1, Journals_mod4,
          se = rob_se,
          digits=3,
          column.labels = c("I", "II","III","IV"),
          type="text")

# (5) Scatterplot 
m <- rbind(c(1, 2), c(3, 0)) 
graphics::layout(m)

plot(Journals$PricePerCitation, Journals$Subscriptions, pch = 20,
     col = "steelblue",
     ylab = "Subscriptions",
     xlab = "ln(Price per ciation)",
     main = "(a)")

# log-log scatter plot
plot(log(Journals$PricePerCitation), log(Journals$Subscriptions), pch = 20,
     col = "steelblue",
     ylab = "ln(Subscriptions)",
     xlab = "ln(Price per ciation)",
     main = "(b)")

# regression line for Model 1
abline(Journals_mod1, lwd = 1.5)

# log-log scatter plot
plot(log(Journals$PricePerCitation), log(Journals$Subscriptions), pch = 20,
     col = "steelblue",
     ylab = "ln(Subscriptions)",
     xlab = "ln(Price per ciation)",
     main = "(c)")

# regression lines for Model 4 for Age = 80
JM4C <-Journals_mod4$coefficients

abline(coef = c(JM4C[1] + JM4C[3] * log(80), JM4C[2] + JM4C[5] * log(80)),
       col = "darkred",
       lwd = 1.5)

# regression lines for Model 4 for Age = 5
abline(coef = c(JM4C[1] + JM4C[3] * log(5), JM4C[2] + JM4C[5] * log(5)),
       col = "darkgreen",
       lwd = 1.5)
####----------------------------------------------------------------------------------

#### 8.4 Nonlinear Regression Models of Test Scores #### 
# 1. Seven different models 
TestScore_mod1 <- lm(score ~ size + english + lunch, data = CASchools)
TestScore_mod2 <- lm(score ~ size + english + lunch + log(income), data = CASchools)
TestScore_mod3 <- lm(score ~ size + HiEL + HiEL:size, data = CASchools)
TestScore_mod4 <- lm(score ~ size + HiEL + HiEL:size + lunch + log(income), data = CASchools)
TestScore_mod5 <- lm(score ~ size + I(size^2) + I(size^3) + HiEL + lunch + log(income),
                     data = CASchools)
TestScore_mod6 <- lm(score ~ size + I(size^2) + I(size^3) + HiEL + HiEL:size +
                       HiEL:I(size^2) + HiEL:I(size^3) + lunch + log(income), data = CASchools)
TestScore_mod7 <- lm(score ~ size + I(size^2) + I(size^3) + english + lunch + log(income), data = CASchools)

rob_se <- list(sqrt(diag(vcovHC(TestScore_mod1, type="HC1"))),
               sqrt(diag(vcovHC(TestScore_mod2, type = "HC1"))), 
               sqrt(diag(vcovHC(TestScore_mod3, type = "HC1"))), 
               sqrt(diag(vcovHC(TestScore_mod4, type = "HC1"))), 
               sqrt(diag(vcovHC(TestScore_mod5, type = "HC1"))), 
               sqrt(diag(vcovHC(TestScore_mod6, type = "HC1"))), 
               sqrt(diag(vcovHC(TestScore_mod7, type = "HC1"))))

stargazer(TestScore_mod1,TestScore_mod2,TestScore_mod3,TestScore_mod4,TestScore_mod5,TestScore_mod6,
          TestScore_mod7, se = rob_se, digits=3,
          dep.var.caption = "Dependent Variable: Test Score",
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"), type="text")

# 2. F-test 
linearHypothesis(TestScore_mod6, c("size:HiEL=0","I(size^2):HiEL=0","I(size^3):HiEL=0"),
                 vcov. = vcovHC, type = "HC1")

# 3. Scatter plot 
plot(CASchools$size, CASchools$score,
     xlim = c(12, 28), ylim = c(600, 740), pch = 20,
     col = "gray",
     xlab = "Student-Teacher Ratio",
     ylab = "Test Score")

# Legend 
legend("top",
       legend = c("Linear Regression (2)",
                  "Cubic Regression (5)","Cubic Regression (7)"),
       cex = 0.8,
       ncol = 3,
       lty = c(1, 1, 2),
       col = c("blue", "red", "black"))

# New data 
new_data <- data.frame("size" = seq(16, 24, 0.05),   # pick sequence between 16-24 by 0.05
                       "english" = mean(CASchools$english),
                       "lunch" = mean(CASchools$lunch), 
                       "income" = mean(CASchools$income), 
                       "HiEL" = mean(CASchools$HiEL))
  
# Regression line for Model 2 with new data 
fitted <- predict(TestScore_mod2, newdata = new_data)

lines(new_data$size, fitted,lwd = 1.5,col = "blue")

# Regression line for Model 5 with new data 
fitted <- predict(TestScore_mod5, newdata = new_data)
lines(new_data$size, fitted,lwd = 1.5,col = "red")

# Regression line for Model 7 with new data 
fitted <- predict(TestScore_mod7, newdata = new_data)
lines(new_data$size, fitted,col = "yellow",
      lwd = 1.5,lty = 2)

# 4. Scatter plot 
# observations with HiEL = 0
plot(CASchools$size[CASchools$HiEL == 0], CASchools$score[CASchools$HiEL == 0], 
     xlim = c(12, 28),
     ylim = c(600, 730),
     pch = 20,
     col = "gray",
     xlab = "Student-Teacher Ratio",
     ylab = "Test Score")

# observations with HiEL = 1
points(CASchools$size[CASchools$HiEL == 1], CASchools$score[CASchools$HiEL == 1], 
       col = "steelblue",
       pch = 20)

# Legend
legend("top",
       legend = c("Regression (6) with HiEL=0", "Regression (6) with HiEL=1"), 
       cex = 0.7,
       ncol = 2,
       lty = c(1, 1),
       col = c("green", "red"))

# New data 
new_data <- data.frame("size" = seq(12, 28, 0.05), 
                       "english" = mean(CASchools$english),
                       "lunch" = mean(CASchools$lunch), 
                       "income" = mean(CASchools$income), "HiEL" = 0)

# Regression line for Model 6 with HiEL=0
fitted <- predict(TestScore_mod6, newdata = new_data)
lines(new_data$size, fitted,lwd = 1.5,col = "green")

# Regression line for Model 6 with HiEL=1
new_data$HiEL <- 1
fitted <- predict(TestScore_mod6, newdata = new_data)
lines(new_data$size, fitted,
      lwd = 1.5,
      col = "red")
####----------------------------------------------------------------------------------