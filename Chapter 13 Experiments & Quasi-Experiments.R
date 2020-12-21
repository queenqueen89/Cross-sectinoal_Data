#### Analysis of the STAR Data #### 
library(AER) 
library(MASS) 
library(mvtnorm) 
library(rddtools) 
library(scales) 
library(stargazer) 
library(dplyr)
library(tidyr)  # 'dplyr' and 'tidyr' for data wrangling functionalities


data(STAR)
head(STAR, 2)   # show first 2 rows
names(STAR)

# 1. Drop NA for the first observation (1st person)
STAR[1, !is.na(STAR[1, ])]   

# 2. Compute differences Estimates for each grades
fmk <- lm(I(readk + mathk) ~ stark, data = STAR) 
fm1 <- lm(I(read1 + math1) ~ star1, data = STAR) 
fm2 <- lm(I(read2 + math2) ~ star2, data = STAR) 
fm3 <- lm(I(read3 + math3) ~ star3, data = STAR)

coeftest(fmk, vcov = vcovHC, type= "HC1")
coeftest(fm1, vcov = vcovHC, type= "HC1")
coeftest(fm2, vcov = vcovHC, type= "HC1")
coeftest(fm3, vcov = vcovHC, type= "HC1")

rob_se_1 <- list(sqrt(diag(vcovHC(fmk, type = "HC1"))), 
                 sqrt(diag(vcovHC(fm1, type = "HC1"))), 
                 sqrt(diag(vcovHC(fm2, type = "HC1"))), 
                 sqrt(diag(vcovHC(fm2, type = "HC1"))))

# 3. Compare the 4 models  
stargazer(fmk,fm1,fm2,fm3,
          title = "Project STAR: Differences Estimates", 
          header = FALSE,
          type = "text",
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3,
          column.labels = c("K", "1", "2", "3"), 
          dep.var.caption = "Dependent Variable: Grade", 
          dep.var.labels.include = FALSE,
          se = rob_se_1)


# 4. Create Subset (with kindergarten data)
# "STARK" is STAR for kindergarten
STARK <- STAR %>%              #  %>% allows to chain function calls        
  transmute(gender, ethnicity, stark, readk, mathk,    # transmute() allows to subset the data set by naming the variables to be kept
            lunchk, experiencek, schoolidk) %>%
  mutate(black = ifelse(ethnicity == "afam", 1, 0),    # mutate() is for adding new variables based on existing ones while preserving the latter
         race = ifelse(ethnicity == "afam" | ethnicity == "cauc", 1, 0), 
         boy = ifelse(gender == "male", 1, 0))

# 5. Estimate three models
gradeK1 <- lm(I(mathk + readk) ~ stark + experiencek, data = STARK)

gradeK2 <- lm(I(mathk + readk) ~ stark + experiencek 
              + schoolidk, data = STARK)

gradeK3 <- lm(I(mathk + readk) ~ stark + experiencek 
              + boy + lunchk + black + race + schoolidk, data = STARK)

coeftest(gradeK1, vcov. = vcovHC, type = "HC1")
coeftest(gradeK2, vcov. = vcovHC, type = "HC1")[1:4, ]
coeftest(gradeK3, vcov. = vcovHC, type = "HC1")[1:7, ]

rob_se_2 <- list(sqrt(diag(vcovHC(fmk, type = "HC1"))), 
                 sqrt(diag(vcovHC(gradeK1, type = "HC1"))), 
                 sqrt(diag(vcovHC(gradeK2, type = "HC1"))), 
                 sqrt(diag(vcovHC(gradeK3, type = "HC1"))))

stargazer(fmk, fm1, fm2, fm3,
          title = "Project STAR - Differences Estimates with Additional Regressors for Kindergarten",
          header = FALSE,
          type = "text",
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3,
          column.labels = c("(1)", "(2)", "(3)", "(4)"),
          dep.var.caption = "Dependent Variable: Test Score in Kindergarten", 
          dep.var.labels.include = FALSE,
          se = rob_se_2)

# 6. Compare 
# Compute sample SD of test scores (for each grade)
SSD <- c("K" = sd(na.omit(STAR$readk + STAR$mathk)), 
         "1" = sd(na.omit(STAR$read1 + STAR$math1)), 
         "2" = sd(na.omit(STAR$read2 + STAR$math2)), 
         "3" = sd(na.omit(STAR$read3 + STAR$math3)))

# (1) For "small" classes 
# Translate effects of small classes to SD
Small <- c("K" = as.numeric(coef(fmk)[2]/SSD[1]), 
           "1" = as.numeric(coef(fm1)[2]/SSD[2]), 
           "2" = as.numeric(coef(fm2)[2]/SSD[3]), 
           "3" = as.numeric(coef(fm3)[2]/SSD[4]))

# Adjusted SE
SmallSE <- c("K" = as.numeric(rob_se_1[[1]][2]/SSD[1]), 
             "1" = as.numeric(rob_se_1[[2]][2]/SSD[2]), 
             "2" = as.numeric(rob_se_1[[3]][2]/SSD[3]), 
             "3" = as.numeric(rob_se_1[[4]][2]/SSD[4]))

# (2) For "aide"
# Translate effects of regular classes with aide to SD
RegAide<- c("K" = as.numeric(coef(fmk)[3]/SSD[1]), 
            "1" = as.numeric(coef(fm1)[3]/SSD[2]), 
            "2" = as.numeric(coef(fm2)[3]/SSD[3]), 
            "3" = as.numeric(coef(fm3)[3]/SSD[4]))

# Adjusted SE
RegAideSE <- c("K" = as.numeric(rob_se_1[[1]][3]/SSD[1]), 
               "1" = as.numeric(rob_se_1[[2]][3]/SSD[2]), 
               "2" = as.numeric(rob_se_1[[3]][3]/SSD[3]), 
               "3" = as.numeric(rob_se_1[[4]][3]/SSD[4]))

# Create Data Frame
df <- t(round(data.frame(
  Small, SmallSE, RegAide, RegAideSE, SSD),
  digits = 2))

# (3) Compare
stargazer(df,
          title = "Estimated Class Size Effects (in Units of Standard Deviations)",
          type = "text",
          summary = FALSE,
          header = FALSE)
####----------------------------------------------------------------------------------

#### DiD Estimator #### 
# 1. Initialize plot and add control group
plot(c(0, 1), c(6, 8), type = "p",
     ylim = c(5, 12),
     xlim = c(-0.3, 1.3),
     main = "The Differences-in-Differences Estimator", 
     xlab = "Period", ylab = "Y",
     col = "steelblue",
     pch = 20,
     xaxt = "n",
     yaxt = "n")

axis(1, at = c(0, 1), labels = c("before", "after")) 
axis(2, at = c(0, 13))

# 2. Add Treatment group
points(c(0, 1, 1), c(7, 9, 11), col = "darkred", pch = 20)

# add line segments
lines(c(0, 1), c(7, 11), col = "darkred")
lines(c(0, 1), c(6, 8), col = "steelblue")
lines(c(0, 1), c(7, 9), col = "darkred", lty = 2) 
lines(c(1, 1), c(9, 11), col = "black", lty = 2, lwd = 2)

# add annotations
text(1, 10, expression(hat(beta)[1]^{DID}), cex = 0.8, pos = 4) 
text(0, 5.5, "s. mean control", cex = 0.8 , pos = 4)
text(0, 6.8, "s. mean treatment", cex = 0.8 , pos = 4)
text(1, 7.9, "s. mean control", cex = 0.8 , pos = 4)
text(1, 11.1, "s. mean treatment", cex = 0.8 , pos = 4)

# 3. Simulate pre- and post-treatment data
n <- 200    # sample size

TEffect <- 4    # define treatment effect

TDummy <- c(rep(0, n/2), rep(1, n/2))   # generate treatment dummy

# 4. Simulate pre- and post-treatment values of the dependent variable
y_pre <- 7 + rnorm(n)

y_pre[1:n/2] <- y_pre[1:n/2] - 1

y_post <- 7 + 2 + TEffect * TDummy + rnorm(n) 

y_post[1:n/2] <- y_post[1:n/2] - 1

pre <- rep(0, length(y_pre[TDummy==0]))
post <- rep(1, length(y_pre[TDummy==0]))

# 5. Plot control group in t = 1
plot(jitter(pre, 0.6), y_pre[TDummy == 0],
     ylim = c(0, 16),
     col = alpha("steelblue", 0.3),
     pch = 20,
     xlim = c(-0.5, 1.5),
     ylab = "Y",
     xlab = "Period",
     xaxt = "n",
     main = "Artificial Data for DID Estimation")

axis(1, at = c(0, 1), labels = c("before", "after"))

# add treatment group in t = 1
points(jitter(pre, 0.6), y_pre[TDummy == 1],
       col = alpha("darkred", 0.3), pch = 20)

# add control group in t = 2
points(jitter(post, 0.6), y_post[TDummy == 0],
       col = alpha("steelblue", 0.5), pch = 20)

# add treatment group in t=2
points(jitter(post, 0.6), y_post[TDummy == 1],
       col = alpha("darkred", 0.5), pch = 20)

# compute the DID estimator for the treatment effect 'by hand'
mean(y_post[TDummy == 1]) - mean(y_pre[TDummy == 1]) - (mean(y_post[TDummy == 0]) - mean(y_pre[TDummy == 0]))

# compute the DID estimator using a linear model
lm(I(y_post - y_pre) ~ TDummy)

# prepare data for DID regression using the interaction term
d <- data.frame("Y" = c(y_pre,y_post),
                "Treatment" = TDummy,
                "Period" = c(rep("1", n), rep("2", n)))

# estimate the model
lm(Y ~ Treatment * Period, data = d)
####----------------------------------------------------------------------------------

#### RD Estimator 
# 1. Create sample data 
W <- runif(1000, -1, 1)    
y <- 3 + 2 * W + 10 * (W>=0) + rnorm(1000)

# 2. Create rdd_data
data <- rdd_data(y, W, cutpoint = 0)

# 3. Plot sample data
plot(data,
     col = "steelblue",
     cex = 0.35,
     xlab = "W",
     ylab = "Y")

# 4. Estimate the sharp RDD model
rdd_mod <- rdd_reg_lm(rdd_object = data, slope = "same")
summary(rdd_mod)

# 5. Plot RDD model along with binned observations
plot(rdd_mod, cex = 0.35,
     col = "steelblue",
     xlab = "W",
     ylab = "Y")

mu <- c(0, 0)
sigma <- matrix(c(1, 0.7, 0.7, 1), ncol = 2)

set.seed(1234)

d <- as.data.frame(mvrnorm(2000, mu, sigma)) 
colnames(d) <- c("W", "Y")

# 6. Introduce fuzziness
d$treatProb <- ifelse(d$W < 0, 0, 0.8)

fuzz <- sapply(X = d$treatProb, 
               FUN = function(x) rbinom(1, 1, prob = x))

# 7. Treatment effect
d$Y <- d$Y + fuzz * 2

# 8. Plot of treatment and control group
plot(d$W, d$Y,
     col = c("steelblue", "darkred")[factor(fuzz)], pch= 20,
     cex = 0.5,
     xlim = c(-3, 3),
     ylim = c(-3.5, 5),
     xlab = "W",
     ylab = "Y")

# add a dashed vertical line at cutoff
abline(v = 0, lty = 2)

# 9. Estimate the Fuzzy RDD 
data <- rdd_data(d$Y, d$W, cutpoint = 0,
                 z = d$treatProb)

frdd_mod <- rdd_reg_lm(rdd_object = data, slope = "same")
frdd_mod

# 10. Plot Fuzzy RDD function
plot(frdd_mod, cex = 0.5, lwd = 0.4,
     xlim = c(-4, 4), ylim = c(-3.5, 5), xlab = "W",
     ylab = "Y")

# 11. Estimate Sharp RDD
data <- rdd_data(d$Y, d$W,
                 cutpoint = 0)
srdd_mod <- rdd_reg_lm(rdd_object = data, slope = "same")
srdd_mod
####----------------------------------------------------------------------------------