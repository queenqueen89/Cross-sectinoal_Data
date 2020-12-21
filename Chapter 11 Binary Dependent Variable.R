#### Binary Dependent Variables and the Linear Probability Model #### 
library(AER) 
library(stargazer)

data(HMDA)
head(HMDA)
summary(HMDA)

# 1. Convert "deny" to numeric
# HMDA$deny is either "yes" or "no"
# as.numeric(HMDA$deny) convert to "1" for "no" or "2" or "yes"
# as.numeric(HMDA$deny)-1 convert to "0" for "no" or "1" or "yes"
HMDA$deny <- as.numeric(HMDA$deny) - 1    

# 2. Simple Linear Probability Model (Simple LPM)
denymod1 <- lm(deny ~ pirat, data = HMDA)    #  dependent variable "deny" is binary
denymod1

# 3. Plot 
plot(x = HMDA$pirat, y = HMDA$deny,
     main = "Scatterplot Mortgage Application Denial and the Payment-to-Income Ratio",
     xlab = "P/I ratio", ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4), cex.main = 0.8)

abline(h = 1, lty = 2, col = "darkred") 
abline(h = 0, lty = 2, col = "darkred") 

text(2.5, 0.9, cex = 0.8, "Mortgage denied") 
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

abline(denymod1, lwd = 1.8, col = "steelblue")

# 4. Robust SE
coeftest(denymod1, vcov. = vcovHC, type = "HC1")

# 5. Rename "afam" to black 
colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"     

# 6. Estimate the model
denymod2 <- lm(deny ~ pirat + black, data = HMDA) 

coeftest(denymod2, vcov. = vcovHC)

####----------------------------------------------------------------------------------

#### Probit Model 
# 1. Probit model (use glm() function, link="probit")
denyprobit <- glm(deny ~ pirat,
                  family = binomial(link = "probit"),
                  data = HMDA) 

coeftest(denyprobit, vcov. = vcovHC, type = "HC1")

# 2. Plot 
plot(x = HMDA$pirat, y = HMDA$deny,
     main = "Probit Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4), cex.main = 0.85)

abline(h = 1, lty = 2, col = "darkred") 
abline(h = 0, lty = 2, col = "darkred") 

text(2.5, 0.9, cex = 0.8, "Mortgage denied") 
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# Add regression line 
x <- seq(0, 3, 0.01)
y <- predict(denyprobit, list(pirat = x), type = "response")
lines(x, y, lwd = 1.5, col = "steelblue")

# 3.Predictions for P/I ratio = 0.3, 0.4
predictions <- predict(denyprobit,
                       newdata = data.frame("pirat" = c(0.3, 0.4)),
                       type = "response") 

# 4. Compute difference in probabilities for P/I ratio = 0.3, 0.4
diff(predictions)

# 5. Probit model: "pirat" +  "black" 
denyprobit2 <- glm(deny ~ pirat + black,
                   family = binomial(link = "probit"),
                   data = HMDA) 

coeftest(denyprobit2, vcov. = vcovHC, type = "HC1")

# 6. Predictions for P/I ratio = 0.3, black = "no" and "yes"
predictions <- predict(denyprobit2,
                       newdata = data.frame("black" = c("no", "yes"),
                                            "pirat" = c(0.3, 0.3)),
                                            type = "response")
# 2. compute difference in probabilities
diff(predictions)
####----------------------------------------------------------------------------------

#### Logit Regression 
# 1. Logit Model (use glm() function, link="logit")
denylogit <- glm(deny ~ pirat,
                 family = binomial(link = "logit"),
                 data = HMDA)

coeftest(denylogit, vcov. = vcovHC, type = "HC1")

# 2. Plot 
plot(x = HMDA$pirat, y = HMDA$deny,
     main = "Probit and Logit Models Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4), cex.main = 0.9)

abline(h = 1, lty = 2, col = "darkred") 
abline(h = 0, lty = 2, col = "darkred") 

text(2.5, 0.9, cex = 0.8, "Mortgage denied") 
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

x <- seq(0, 3, 0.01)
y_probit <- predict(denyprobit, list(pirat = x), type = "response") 
y_logit <- predict(denylogit, list(pirat = x), type = "response")
lines(x, y_probit, lwd = 1.5, col = "steelblue") 
lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)

# Legend
legend("topleft", horiz = TRUE,
       legend = c("Probit", "Logit"), col = c("steelblue", "black"), lty = c(1, 2))

# 3. Logit Model: "pirat" + "black"
denylogit2 <- glm(deny ~ pirat + black,
                  family = binomial(link = "logit"),
                  data = HMDA) 

coeftest(denylogit2, vcov. = vcovHC, type = "HC1")

# 4. Predictions for P/I ratio = 0.3
predictions <- predict(denylogit2, 
                       newdata = data.frame("black" = c("no","yes"),
                                          "pirat" = c(0.3,0.3)),
                                          type = "response")

diff(predictions)
####----------------------------------------------------------------------------------

#### Measures of Fit #### 
# Compute pseudo-R2 for the Probit Model 
# Method 1
pseudoR2 <- 1 - (denyprobit2$deviance) / (denyprobit2$null.deviance)    # similar to 1-SSR/SST
pseudoR2

# Method 2
# (1) Compute the null model
denyprobit_null <- glm(formula = deny ~ 1,
                       family = binomial(link = "probit"),
                       data = HMDA)

# (2) Compute the pseudo-R2 using 'logLik'
1 - logLik(denyprobit2)[1]/logLik(denyprobit_null)[1]

####----------------------------------------------------------------------------------

#### LPM, Logt & Probit Models #### 

# 1. Mean for different variables 
# (1) Mean for "P/I ratio"
mean(HMDA$pirat)

# (2) Mean for "inhouse expense-to-total-income ratio"
mean(HMDA$hirat)

# (3) Mean for "loan-to-value ratio"
mean(HMDA$lvrat)

# (4) Mean for "consumer credit score"
mean(as.numeric(HMDA$chist))

# (5) Mean for "mortgage credit score"
mean(as.numeric(HMDA$mhist))

# (6) Mean for public bad credit record
mean(as.numeric(HMDA$phist)-1)

# (7) Proportion of "yes' & "no" in "denied mortgage insurance"
# table() shows how many "yes" & "no"; prop.table(table()) shows proportin of "yes" & "no"
prop.table(table(HMDA$insurance)) 

# (8) Proportion of "yes' & "no" in "self-employed"
prop.table(table(HMDA$selfemp))

# (9) Proportion of "yes' & "no" in "single"
prop.table(table(HMDA$single))

# (10) Proportion of "yes' & "no" in "high school diploma" 
prop.table(table(HMDA$hschool))

# (11) Mean for "unemployment rate" 
mean(HMDA$unemp)

# (12) Proportion of "yes' & "no" in "condominium" 
prop.table(table(HMDA$condomin))

# (13) Proportion of "yes' & "no" in "black"
prop.table(table(HMDA$black))

# (14) Proportion of "yes' & "no" in "deny" 
prop.table(table(HMDA$deny))

# 2. Transform Variables 
# (1) convert "lvrat" from numeric into factor 
HMDA$lvrat <- factor(
        ifelse(HMDA$lvrat < 0.8, "low",
        ifelse(HMDA$lvrat >= 0.8 & HMDA$lvrat <= 0.95, "medium", "high")), 
        levels = c("low", "medium", "high"))

# (2) convert "mhist" & "chist" from factor to numeric
HMDA$mhist <- as.numeric(HMDA$mhist) 
HMDA$chist <- as.numeric(HMDA$chist)

# 3. Six Models
# Linear Probability Model (LPM)
lpm_HMDA <- lm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist + insurance + selfemp, 
               data = HMDA)

# Logit Model 
logit_HMDA <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist + insurance + selfemp,
                  family = binomial(link = "logit"), data = HMDA)

# Probit Models 
probit_HMDA_1 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist + insurance + selfemp,
                     family = binomial(link = "probit"), data = HMDA)

probit_HMDA_2 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist + insurance + selfemp 
                     + single + hschool + unemp,
                     family = binomial(link = "probit"), data = HMDA)

probit_HMDA_3 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist + insurance + selfemp 
                     + single + hschool + unemp + condomin
                     + I(mhist==3) + I(mhist==4) + I(chist==3) + I(chist==4) + I(chist==5) + I(chist==6),
                     family = binomial(link = "probit"),
                     data = HMDA)

probit_HMDA_4 <- glm(deny ~ black * (pirat + hirat) + lvrat + chist + mhist + phist + insurance + selfemp 
                     + single + hschool + unemp,
                     family = binomial(link = "probit"), data = HMDA)

rob_se <- list(sqrt(diag(vcovHC(lpm_HMDA, type = "HC1"))), 
               sqrt(diag(vcovHC(logit_HMDA, type = "HC1"))),
               sqrt(diag(vcovHC(probit_HMDA_1, type = "HC1"))), 
               sqrt(diag(vcovHC(probit_HMDA_2, type = "HC1"))), 
               sqrt(diag(vcovHC(probit_HMDA_3, type = "HC1"))), 
               sqrt(diag(vcovHC(probit_HMDA_4, type = "HC1"))))

stargazer(lpm_HMDA, logit_HMDA, probit_HMDA_1,
          probit_HMDA_2, probit_HMDA_3, probit_HMDA_4,
          digits = 3,
          type = "text",
          header = FALSE,
          se = rob_se,
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"))

# 4. Find Denial Probability Differences between non-black and black (with mean values)
# (1) Create New Data Frame
new <- data.frame(
        "pirat" = mean(HMDA$pirat), 
        "hirat" = mean(HMDA$hirat), 
        "lvrat" = "low",
        "chist" = mean(HMDA$chist), 
        "mhist" = mean(HMDA$mhist), 
        "phist" = "no", 
        "insurance" = "no", 
        "selfemp" = "no",
        "black" = c("no", "yes"), 
        "single" = "no",
        "hschool" = "yes",
        "unemp" = mean(HMDA$unemp), 
        "condomin" = "no")

# (2) Difference predicted by LPM
predictions <- predict(lpm_HMDA, newdata = new) 
diff(predictions)

# (2) Difference predicted by the logit model
predictions <- predict(logit_HMDA, newdata = new, type = "response") 
diff(predictions)

# (3) Difference predicted by probit model (3)
predictions <- predict(probit_HMDA_1, newdata = new, type = "response") 
diff(predictions)

# (4) Difference predicted by probit model (4)
predictions <- predict(probit_HMDA_2, newdata = new, type = "response") 
diff(predictions)

# (5) Difference predicted by probit model (5)
predictions <- predict(probit_HMDA_3, newdata = new, type = "response") 
diff(predictions)

# (6) Difference predicted by probit model (6)
predictions <- predict(probit_HMDA_4, newdata = new, type = "response") 
diff(predictions)

# 5. Robust F test
linearHypothesis(probit_HMDA_4, test = "F",
                 c("blackyes=0", "blackyes:pirat=0", "blackyes:hirat=0"), vcov = vcovHC, type = "HC1")
####----------------------------------------------------------------------------------