#### IV: Application to the Demand For Cigarettes #### 

library(AER) 
library(stargazer)

data("CigarettesSW") 
summary(CigarettesSW)

# 1. Create new variable 
# (1) "rprice" = real per capita prices
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

# (2) "salestax"
CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

# (3) check correlation between sales tax and price
cor(CigarettesSW$salestax, CigarettesSW$price)

# 2. Create Subset Data 
c1995 <- subset(CigarettesSW, year == "1995")

# 3. IV Model
# Method 1: run 1st stage & 2nd stage separately
# (1) First Stage regression
cig_s1 <- lm(log(rprice) ~ salestax, data = c1995)   # IV: salestax

coeftest(cig_s1, vcov = vcovHC, type = "HC1")

summary(cig_s1)$r.squared             # get R2

lcigp_pred <- cig_s1$fitted.values    # get fitted values 

# (2) Second Stage regression
cig_s2 <- lm(log(c1995$packs) ~ lcigp_pred) 

coeftest(cig_s2, vcov = vcovHC)

# Method 2: use ivreg() function
cig_ivreg <- ivreg(log(packs) ~ log(rprice) | salestax, data = c1995)   # IV: salestax

coeftest(cig_ivreg, vcov = vcovHC, type = "HC1")
####----------------------------------------------------------------------------------

#### IV: Application to the Demand for Cigarettes #### 

# 1. Create new variable & Data 
# (1) "rincome" = real income per capita 
CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi) 

# (2) Create Subset
c1995 <- subset(CigarettesSW, year == "1995")

# 2. IV Regression 
# "log(rincome)" exogenous, "saletax" is VI
cig_ivreg2 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + salestax, data = c1995)

coeftest(cig_ivreg2, vcov = vcovHC, type = "HC1")

# 3. Create new variable & Data 
CigarettesSW$cigtax <- with(CigarettesSW, tax/cpi) 

c1995 <- subset(CigarettesSW, year == "1995")

# 4. IV Regression 
# "log(rincome)" exogenous, "saletax" and "cigtax" are VI
cig_ivreg3 <- ivreg(log(packs) ~ log(rprice) + log(rincome) 
                    | log(rincome) + salestax + cigtax, data = c1995)

coeftest(cig_ivreg3, vcov = vcovHC, type = "HC1")

####----------------------------------------------------------------------------------

#### Check Instrument Validity: Demand for Cigarettes #### 
# 1. Subset data for year 1985
c1985 <- subset(CigarettesSW, year == "1985") 

# 2. Define differences in variables
packsdiff <- log(c1995$packs) - log(c1985$packs)
pricediff <- log(c1995$price/c1995$cpi) - log(c1985$price/c1985$cpi)
incomediff <- log(c1995$income/c1995$population/c1995$cpi) - log(c1985$income/c1985$population/c1985$cpi)
salestaxdiff <- (c1995$taxs - c1995$tax)/c1995$cpi - (c1985$taxs - c1985$tax)/c1985$cpi 
cigtaxdiff <- c1995$tax/c1995$cpi - c1985$tax/c1985$cpi

# 3. Three IV Models (with differenced variables) 
# Model 1: "pricediff" endogenous, "incomediff" exogenous, "salestaxdiff" & "cigtaxdiff" are IVs
cig_ivreg_diff1 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + salestaxdiff)   # variables behind | are exogenous
coeftest(cig_ivreg_diff1, vcov = vcovHC, type = "HC1")

# Model 2
cig_ivreg_diff2 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + cigtaxdiff)
coeftest(cig_ivreg_diff2, vcov = vcovHC, type = "HC1")

# Model 3
cig_ivreg_diff3 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + salestaxdiff + cigtaxdiff)
coeftest(cig_ivreg_diff3, vcov = vcovHC, type = "HC1")

rob_se <- list(sqrt(diag(vcovHC(cig_ivreg_diff1, type = "HC1"))), 
               sqrt(diag(vcovHC(cig_ivreg_diff2, type = "HC1"))), 
               sqrt(diag(vcovHC(cig_ivreg_diff3, type = "HC1"))))

stargazer(cig_ivreg_diff1, cig_ivreg_diff2,cig_ivreg_diff3, header = FALSE,
          type = "html",
          omit.table.layout = "n",
          digits = 3,
          column.labels = c("IV: salestax", "IV: cigtax", "IVs: salestax, cigtax"), 
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent Variable: 1985-1995 Difference in Log per Pack Price", 
          se = rob_se)

# 4. First Stage Regressions
mod_relevance1 <- lm(pricediff ~ incomediff + salestaxdiff) 

mod_relevance2 <- lm(pricediff ~ cigtaxdiff + incomediff) 

mod_relevance3 <- lm(pricediff ~ incomediff + salestaxdiff + cigtaxdiff)

# 5. Check IV relevance 
# IV relevance for model (1)
linearHypothesis(mod_relevance1, "salestaxdiff = 0",
                 vcov = vcovHC, type = "HC1")

# IV relevance for model (2)
linearHypothesis(mod_relevance2, "cigtaxdiff = 0",
                 vcov = vcovHC, type = "HC1")

# IV relevance for model (3)
linearHypothesis(mod_relevance3,
                 c("salestaxdiff = 0", "cigtaxdiff = 0"),
                 vcov = vcovHC, type = "HC1")

# 6. Compute the J-statistic
cig_iv_OR <- lm(residuals(cig_ivreg_diff3) ~ incomediff + salestaxdiff + cigtaxdiff)

cig_OR_test <- linearHypothesis(cig_iv_OR,
                                c("salestaxdiff = 0", "cigtaxdiff = 0"),
                                test = "Chisq")
cig_OR_test

# 7. Compute correct p-value for J-statistic
pchisq(cig_OR_test[2, 5], df = 1, lower.tail = FALSE)
####----------------------------------------------------------------------------------