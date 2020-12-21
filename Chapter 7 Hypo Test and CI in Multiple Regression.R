#### Hypo Test and CI for single coefficient ####
library(AER)
library(stargazer)

# 1. Test significance of coefficients (use coeftest() function)
data(CASchools)
CASchools$score <- (CASchools$read + CASchools$math)/2
CASchools$STR <- CASchools$students/CASchools$teachers     # student/teacher ratio(size)

model <- lm(score ~ STR + english, data = CASchools)
coeftest(model, vcov. = vcovHC, type = "HC1")

# 2. Computer 2-sided p-value manually 
2*(1-pt(abs(coeftest(model, vcov. = vcovHC, type = "HC1")[2,3]), df=model$df.residual))
####----------------------------------------------------------------------------------

#### Application to Test Scores and Student-Teacher Ratio ####
# 1. CI (use confint() function)
model <- lm(score ~ STR + english, data = CASchools)

# (1) Default 95% CI
confint(model)      

# (2) Specify 90% CI
confint(model, level = 0.9)      

# 2. CI with Robust SE
rob_se <- diag(vcovHC(model, type="HC1"))^0.5     # find robust se

rbind("lower" = coef(model) - qnorm(0.975) * rob_se,
      "upper" = coef(model) + qnorm(0.975) * rob_se)

# 3. Add variable "expenditure"
CASchools$expenditure <- CASchools$expenditure/1000

model <- lm(score ~ STR + english + expenditure, data=CASchools)
coeftest(model, vcov. = vcovHC, type="HC1")

# 4. Correlation between expenditure and STR
cor(CASchools$STR, CASchools$expenditure)
####----------------------------------------------------------------------------------

#### Joint Hypo Testing using F-stat ####
# 1. F-stat: if coefficients for STR, expenditure significant
model <- lm(score ~ STR + english + expenditure, data=CASchools)

linearHypothesis(model, c("STR=0", "expenditure=0"))    

# 2. F-stat with hetero-robust
linearHypothesis(model, c("STR=0", "expenditure=0"), white.adjust = "hc1") 

# 3. F-stat: if coefficients for STR, expenditure, english significant
linearHypothesis(model, c("STR=0", "expenditure=0", "english=0"))   

# 4. Full model F-stat
summary(model)$fstatistic
####----------------------------------------------------------------------------------

#### Confidence sets for multiple coefficients ####
# 1. Draw 95% confidence set for coefficients (use confidenceEllipse() function)
confidenceEllipse(model, fill = T,
                  lwd = 0,
                  which.coef = c("STR", "expenditure"), main = "95% Confidence Set")

# 2. Draw robust 95% confidence set
confidenceEllipse(model,
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"),
                  main = "95% Confidence Sets",
                  vcov. = vcovHC(model, type = "HC1"),
                  col = "red")
# draw the 95% confidence set for coefficients on size and expenditure
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"), add = T)
####----------------------------------------------------------------------------------

#### Model Specification for Multiple Regression ####
# 1. Add "lunch" variable 
model <- lm(score ~ STR + english + lunch, data = CASchools) 
coeftest(model, vcov. = vcovHC, type = "HC1")

# 2. Generate observations for parking lot space
set.seed(1)

CASchools$PLS <- c(22 * CASchools$income - 15 * CASchools$STR
                   + 0.2 * CASchools$expenditure
                   + rnorm(nrow(CASchools), sd = 80) + 3000)

plot(CASchools$PLS, CASchools$score,
     xlab = "Parking Lot Space",
     ylab = "Test Score",
     pch = 20,
     col = "steelblue")

summary(lm(score ~ PLS, data = CASchools))
####----------------------------------------------------------------------------------

#### Analysis of the Test Score Data Set #### 
# 1. Correlation between calworks and lunch 
cor(CASchools$calworks, CASchools$lunch)

# 2. Scatter plot for english/lunch/calworks against score
m <- rbind(c(1, 2), c(3, 0)) 
graphics::layout(mat = m)

plot(score ~ english, data = CASchools,
     col = "steelblue",
     pch = 20,
     xlim = c(0, 100),
     cex.main = 0.9,
     main = "Percentage of English language learners")
plot(score ~ lunch, data = CASchools,
     col = "steelblue",
     pch = 20,
     cex.main = 0.9,
     main = "Percentage qualifying for reduced price lunch")
plot(score ~ calworks, data = CASchools,
     col = "steelblue",
     pch = 20,
     xlim = c(0, 100),
     cex.main = 0.9,
     main = "Percentage qualifying for income assistance")

# 3. Correlations 
cor(CASchools$score, CASchools$english)
cor(CASchools$score, CASchools$lunch)
cor(CASchools$score, CASchools$calworks)

# 4. Compare 5 different models 
library(stargazer)

spec1 <- lm(score ~ STR, data = CASchools) 
spec2 <- lm(score ~ STR + english, data = CASchools)
spec3 <- lm(score ~ STR + english + lunch, data = CASchools)
spec4 <- lm(score ~ STR + english + calworks, data = CASchools)
spec5 <- lm(score ~ STR + english + lunch + calworks, data = CASchools)

rob_se <- list(sqrt(diag(vcovHC(spec1, type="HC1"))), 
               sqrt(diag(vcovHC(spec2, type="HC1"))), 
               sqrt(diag(vcovHC(spec3, type="HC1"))), 
               sqrt(diag(vcovHC(spec4, type="HC1"))), 
               sqrt(diag(vcovHC(spec5, type="HC1"))))

stargazer(spec1, spec2, spec3,spec4,spec5,
          se = rob_se,
          digits=3,
          header=F,
          column.labels=c("(I)", "(II)", "(III)", "(IV)", "(V)"),
          type="text")
####----------------------------------------------------------------------------------