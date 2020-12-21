#### Traffic death and alcohol taxes 
library(AER) 
library(plm) 
library(stargazer)

# 1. Traffic Deaths and Alcohol Taxes
# (1) Analyze data 
data(Fatalities)

is.data.frame(Fatalities)    # check if it's data frame

dim(Fatalities)

str(Fatalities)      # display structure of the data set  

head(Fatalities)

summary(Fatalities[, c(1, 2)])    # summary for first  2 columns of data 

# (2) 

# Create New Variables 
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# Subset the data
Fatalities1982 <- subset(Fatalities, year == "1982") 
Fatalities1988 <- subset(Fatalities, year == "1988")

# Regression for 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982) 
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)

coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")
coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")

# Scatter Plot for 1982 data
plot(x = Fatalities1982$beertax,
     y = Fatalities1982$fatal_rate,
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982", ylim = c(0, 4.5),
     pch = 20,
     col = "steelblue")
abline(fatal1982_mod, lwd = 1.5)

# Scatter Plot for 1988 data
plot(x = Fatalities1988$beertax,
     y = Fatalities1988$fatal_rate,
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988", ylim = c(0, 4.5),
     pch = 20,
     col = "steelblue")
abline(fatal1988_mod, lwd = 1.5)
####----------------------------------------------------------------------------------

#### Panel Data: DiD Model #### 
# (1) Find fatality/beertax differences between 1988 and 1982 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate 
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# (2) DiD Model 
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax) 
coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")

# (3) Scatter Plot 
plot(x = diff_beertax,
     y = diff_fatal_rate,
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988", xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20,
     col = "steelblue")
abline(fatal_diff_mod, lwd = 1.5)

mean(Fatalities$fatal_rate)
####----------------------------------------------------------------------------------

#### FE Model with State Fixed Effect ####
# Method 1: use lm() add "state" variable 
fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)   # The lm() converts factors into dummies automatically

coeftest(fatal_fe_lm_mod, vcov. = vcovHC, type = "HC1")[1,]

# Method 2: use plm() oneway
fatal_fe_mod <- plm(fatal_rate ~ beertax, data = Fatalities,
                    index = c("state", "year"), model = "within")
coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")[1,]     # method 1 and 2 get same coefficients but different SE

# Method 3: demeaned
fat_demeaned <- with(Fatalities, 
                       data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
                                       beertax = beertax - ave(beertax, state)))

fatal_demeaned <- lm(fatal_rate ~ beertax - 1, data = fat_demeaned)
summary(fatal_demeaned)
####----------------------------------------------------------------------------------

#### FE Model with Time Fixed Effect ####
# Method 1: use lm() add "state + year" variable 
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities) 

coeftest(fatal_tefe_lm_mod, vcov. = vcovHC, type = "HC1")[1,]

# Method 2: use plm() twoways
fatal_tefe_mod <- plm(fatal_rate ~ beertax, data = Fatalities,
                      index = c("state", "year"), model = "within",
                      effect = "twoways")
coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")[1,]


####----------------------------------------------------------------------------------

#### Fix the OVB Problem 
# 1. Create Variables 
# Discretize the minimum legal drinking age
Fatalities$drinkagec <- cut(Fatalities$drinkage, breaks = 18:22,    # cut() drinking age by age 18,19,20,21
                            include.lowest = TRUE,
                            right = FALSE)
summary(Fatalities$drinkagec)

# Set minimum drinking age [21, 22] to be the baseline level
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")   # relevel() set minimum drinking age

# Mandatory jail or community service?
Fatalities$punish <- with(Fatalities, 
                          factor(jail == "yes" | service == "yes", labels = c("no", "yes")))

# New data only for 1982 and 1988
Fatalities_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988), ]

# 2. Seven different models
fatalities_mod1 <- lm(fatal_rate ~ beertax, data = Fatalities)

fatalities_mod2 <- plm(fatal_rate ~ beertax + state, data = Fatalities)   # state fixed effect 

fatalities_mod3 <- plm(fatal_rate ~ beertax + state + year,    # state and time fixed effects 
                       index = c("state","year"),
                       model = "within", effect = "twoways", data = Fatalities)

fatalities_mod4 <- plm(fatal_rate ~ beertax + state + year      # state and time fixed effects and more
                       + drinkagec + punish + miles + unemp + log(income),
                       index = c("state", "year"), model = "within",
                       effect = "twoways", data = Fatalities)

fatalities_mod5 <- plm(fatal_rate ~ beertax + state + year   # state and time fixed effects and more 
                       + drinkagec + punish + miles,
                       index = c("state", "year"), model = "within",
                       effect = "twoways", data = Fatalities)

fatalities_mod6 <- plm(fatal_rate ~ beertax + year        # time fixed effects and more 
                       + drinkage+ punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within", effect = "twoways", data = Fatalities)

fatalities_mod7 <- plm(fatal_rate ~ beertax + state + year   # state and time fixed effects and more 
                       + drinkagec + punish + miles + unemp + log(income),
                       index = c("state", "year"), model = "within",
                       effect = "twoways",
                       data = Fatalities_1982_1988)          # data for 1982 and 1988 

rob_se <- list(sqrt(diag(vcovHC(fatalities_mod1, type = "HC1"))), 
               sqrt(diag(vcovHC(fatalities_mod2, type = "HC1"))), 
               sqrt(diag(vcovHC(fatalities_mod3, type = "HC1"))), 
               sqrt(diag(vcovHC(fatalities_mod4, type = "HC1"))), 
               sqrt(diag(vcovHC(fatalities_mod5, type = "HC1"))), 
               sqrt(diag(vcovHC(fatalities_mod6, type = "HC1"))), 
               sqrt(diag(vcovHC(fatalities_mod7, type = "HC1"))))

stargazer(fatalities_mod1, fatalities_mod2, fatalities_mod3,
          fatalities_mod4, fatalities_mod5, fatalities_mod6, fatalities_mod7,
          digits = 3,
          header = FALSE,
          type = "text",
          se = rob_se,
          title = "Linear Panel Regression Models of Traffic Fatalities due to Drunk Driving",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"))

# 3. Test if legal drinking age has no explanatory power
linearHypothesis(fatalities_mod4, test = "F",
                 c("drinkagec[18,19)=0", "drinkagec[19,20)=0", "drinkagec[20,21)"), 
                 vcov. = vcovHC, type = "HC1")

# 4. Test if economic indicators have no explanatory power
linearHypothesis(fatalities_mod4, test = "F",
                 c("log(income)", "unemp"), vcov. = vcovHC, type = "HC1")   # economic variables significantly explain fatalities
####----------------------------------------------------------------------------------