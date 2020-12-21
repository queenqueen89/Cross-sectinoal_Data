#### Dynamic causal effects: The Orange Juice Data #### 
library(AER) 
library(quantmod) 
library(dynlm) 
library(orcutt) 
library(nlme) 
library(stargazer)
library(orcutt)

# load the frozen orange juice data set
data("FrozenJuice")

# compute the price index for frozen concentrated juice
FOJCPI <- FrozenJuice[, "price"]/FrozenJuice[, "ppi"] 

FOJC_pctc <- 100 * diff(log(FOJCPI))

FDD <- FrozenJuice[, "fdd"]

# convert series to xts objects
FOJCPI_xts <- as.xts(FOJCPI) 

FDD_xts <- as.xts(FrozenJuice[, 3])

# Plot orange juice price index
plot(as.zoo(FOJCPI), col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Price index",
     main = "Frozen Concentrated Orange Juice")

# divide plotting area
par(mfrow = c(2, 1))

# Plot percentage changes in prices
plot(as.zoo(FOJC_pctc), col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Percent",
     main = "Monthly Changes in the Price of Frozen Conentrated Orange Juice")

# plot freezing degree days
plot(as.zoo(FDD),
     col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Freezing degree days",
     main = "Monthly Freezing Degree Days in Orlando, FL")

# simple regression of percentage changes on freezing degree days
orange_SR <- dynlm(FOJC_pctc ~ FDD) 

coeftest(orange_SR, vcov. = vcovHAC)

# distributed lag model with 6 lags of freezing degree days
orange_DLM <- dynlm(FOJC_pctc ~ FDD + L(FDD, 1:6)) 

coeftest(orange_DLM, vcov. = vcovHAC)
####----------------------------------------------------------------------------------

#### Dynamic Multipliers and Cumulative Dynamic Multipliers #### 
# compute cumulative multipliers
cum_mult <-cumsum(orange_DLM$coefficients[-1]) # rename entries

names(cum_mult) <- paste(0:6, sep = "-", "period CDM") 
cum_mult

# estimate cumulative dynamic multipliers using the modified regression
cum_mult_reg <-dynlm(FOJC_pctc ~ d(FDD) + d(L(FDD,1:5)) + L(FDD,6)) 

coef(cum_mult_reg)[-1]

# obtain coefficient summary that reports HAC standard errors
coeftest(cum_mult_reg, vcov. = vcovHAC)

# function that computes rho tilde
acf_c <- function(x, j) { return(
  t(x[-c(1:j)]) %*% na.omit(Lag(x, j)) / t(x) %*% x )
}

# simulate time series with serially correlated errors
set.seed(1) N <- 100

eps <- arima.sim(n = N, model = list(ma = 0.5)) 

X <- runif(N, 1, 10)
Y <- 0.5 * X + eps

# compute OLS residuals
res <- lm(Y ~ X)$res # compute v
v <- (X - mean(X)) * res

# compute robust estimate of beta_1 variance
var_beta_hat <- 1/N * (1/(N-2) * sum((X - mean(X))^2 * res^2) ) / (1/N * sum((X - mean(X))^2))^2

# rule of thumb truncation parameter
m <- floor(0.75 * N^(1/3))

# compute correction factor
f_hat_T <- 1 + 2 * sum((m - 1:(m-1))/m * sapply(1:(m - 1), function(i) acf_c(x = v, j = i)) )

# compute Newey-West HAC estimate of the standard error
sqrt(var_beta_hat * f_hat_T)

# Using NeweyWest():
NW_VCOV <- NeweyWest(lm(Y ~ X),
                     lag = m - 1, prewhite = F,
                     adjust = T)

# compute standard error
sqrt(diag(NW_VCOV))[2]

example_mod <- lm(Y ~ X) 
coeftest(example_mod, vcov = NW_VCOV)

# set seed for reproducibility
set.seed(1)

# simulate a time series with serially correlated errors
obs <- 501
eps <- arima.sim(n = obs-1 , model = list(ar = 0.5)) 

X <- arima.sim(n = obs, model = list(ar = 0.25))
Y <- 0.1 * X[-1] + 0.25 * X[-obs] + eps
X <- ts(X[-1])

# estimate the distributed lag model
dlm <- dynlm(Y ~ X + L(X))

# check that the residuals are serially correlated
acf(residuals(dlm))

# coefficient summary using the Newey-West SE estimates
coeftest(dlm, vcov = NeweyWest, prewhite = F, adjust = T)

# estimate the ADL(2,1) representation of the distributed lag model
adl21_dynamic <- dynlm(Y ~ L(Y) + X + L(X, 1:2))

# plot the sample autocorrelaltions of residuals
acf(adl21_dynamic$residuals)

# compute estimated dynamic effects using coefficient restrictions # in the ADL(2,1) representation
t <- adl21_dynamic$coefficients

c("hat_beta_1" = t[3],
  "hat_beta_2" = t[4] + t[3] * t[2])

# GLS: estimate quasi-differenced specification by OLS
iGLS_dynamic <- dynlm(I(Y- 0.5 * L(Y)) ~ I(X - 0.5 * L(X)) + I(L(X) - 0.5 * L(X, 2))) 

summary(iGLS_dynamic)

X_t <- c(X[-1])

# create first lag 
X_l1 <- c(X[-500]) 
Y_t <- c(Y[-1])

# iterated cochrane-orcutt procedure
summary(cochrane.orcutt(lm(Y_t ~ X_t + X_l1)))

# feasible GLS maximum likelihood estimation procedure
summary(gls(Y_t ~ X_t + X_l1, correlation = corAR1()))

# estimate distributed lag models of frozen orange juice price changes
FOJC_mod_DM <- dynlm(FOJC_pctc ~ L(FDD, 0:18))
FOJC_mod_CM1 <- dynlm(FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18))
FOJC_mod_CM2 <- dynlm(FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18) + season(FDD))

# set lag orders as regressor labels
attr(FOJC_mod_DM$coefficients, "names")[1:20] <- c("(Intercept)", 
                                                   as.character(0:18)) 

attr(FOJC_mod_CM1$coefficients, "names")[1:20] <- c("(Intercept)", 
                                                    as.character(0:18)) 

attr(FOJC_mod_CM2$coefficients, "names")[1:20] <- c("(Intercept)", 
                                                    as.character(0:18))

length(FDD)

# gather HAC standard error errors in a list
SEs <- list(sqrt(diag(NeweyWest(FOJC_mod_DM, lag = 7, prewhite = F))), 
            sqrt(diag(NeweyWest(FOJC_mod_CM1, lag = 7, prewhite = F))), 
            sqrt(diag(NeweyWest(FOJC_mod_CM1, lag = 14, prewhite = F))), 
            sqrt(diag(NeweyWest(FOJC_mod_CM2, lag = 7, prewhite = F))))

stargazer(FOJC_mod_DM , FOJC_mod_CM1, FOJC_mod_CM1, FOJC_mod_CM2,
          title = "Dynamic Effects of a Freezing Degree Day on the Price of Orange Juice",
          header = FALSE,
          digits = 3,
          column.labels = c("Dynamic Multipliers", rep("Dynamic Cumulative Multipliers", 3)), 
          dep.var.caption = "Dependent Variable: Monthly Percentage Change in Orange Juice Price", 
          dep.var.labels.include = FALSE,
          covariate.labels = as.character(0:18), 
          omit = "season",
          se = SEs,
          no.space = NULL,
          add.lines = list(c("Monthly indicators?","no", "no", "no", "yes"), 
                           c("HAC truncation","7", "7", "14", "7")),
          omit.stat = c("rsq", "f","ser"),
          type="text")

# estimates on monthly dummies
FOJC_mod_CM2$coefficients[-c(1:20)]

# test if coefficients on monthly dummies are zero
unres_model <- dynlm(FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18) + season(FDD))

res_model <- update(unres_model, formula = . ~ . - season(FDD))

waldtest(unres_model, res_model,
         vcov = NeweyWest(unres_model, lag = 7, prewhite = F))

# 95% CI bounds
point_estimates <- FOJC_mod_DM$coefficients

CI_bounds <- cbind("lower" = point_estimates - 1.96 * SEs[[1]],
                   "upper" = point_estimates + 1.96 * SEs[[1]])[-1, ]

# plot the estimated dynamic multipliers
plot(0:18, point_estimates[-1], type = "l",
     lwd = 2,
     col = "steelblue",
     ylim = c(-0.4, 1),
     xlab = "Lag",
     ylab = "Dynamic multiplier",
     main = "Dynamic Effect of FDD on Orange Juice Price")

# add a dashed line at 0
abline(h = 0, lty = 2)

# add CI bounds
lines(0:18, CI_bounds[,1], col = "darkred") 
lines(0:18, CI_bounds[,2], col = "darkred")

# 95% CI bounds
point_estimates <- FOJC_mod_CM1$coefficients

CI_bounds <- cbind("lower" = point_estimates - 1.96 * SEs[[2]],
                   "upper" = point_estimates + 1.96 * SEs[[2]])[-1,]

# plot estimated dynamic multipliers
plot(0:18, point_estimates[-1], type = "l",
     lwd = 2,
     col = "steelblue",
     ylim = c(-0.4, 1.6),
     xlab = "Lag",
     ylab = "Cumulative dynamic multiplier",
     main = "Cumulative Dynamic Effect of FDD on Orange Juice Price")

# add dashed line at 0
abline(h = 0, lty = 2)

# add CI bounds
lines(0:18, CI_bounds[, 1], col = "darkred") 
lines(0:18, CI_bounds[, 2], col = "darkred")

# estimate cumulative multiplieres using different sample periods
FOJC_mod_CM1950 <- update(FOJC_mod_CM1, start = c(1950, 1), end = c(1966, 12)) 
FOJC_mod_CM1967 <- update(FOJC_mod_CM1, start = c(1967, 1), end = c(1983, 12)) 
FOJC_mod_CM1984 <- update(FOJC_mod_CM1, start = c(1984, 1), end = c(2000, 12))

# plot estimated dynamic cumulative multipliers (1950-1966)
plot(0:18, FOJC_mod_CM1950$coefficients[-1], type = "l",
     lwd = 2,
     col = "steelblue",
     xlim = c(0, 20),
     ylim = c(-0.5, 2),
     xlab = "Lag",
     ylab = "Cumulative dynamic multiplier",
     main = "Cumulative Dynamic Effect for Different Sample Periods")

# plot estimated dynamic multipliers (1967-1983)
lines(0:18, FOJC_mod_CM1967$coefficients[-1], lwd = 2) # plot estimated dynamic multipliers (1984-2000)
lines(0:18, FOJC_mod_CM1984$coefficients[-1], lwd = 2, col = "darkgreen") # add dashed line at 0

abline(h = 0, lty = 2)

# add annotations
text(18, -0.24, "1984 - 2000") 
text(18, 0.6, "1967 - 1983") 
text(18, 1.2, "1950 - 1966")

# set up a range of possible break dates
tau <- c(window(time(FDD), time(FDD)[round(612/100*15)],
                time(FDD)[round(612/100*85)]))     # initialize the vector of F-statistics

Fstats <- numeric(length(tau)) # the restricted model

res_model <- dynlm(FOJC_pctc ~ L(FDD, 0:18)) # estimation, loop over break dates

for(i in 1:length(tau)) {            
  D <- time(FOJC_pctc) > tau[i]        # set up dummy variable
  unres_model <- dynlm(FOJC_pctc ~ D * L(FDD, 0:18))      # estimate DL model with intercations
  Fstats[i] <- waldtest(res_model, unres_model,      # compute and save F-statistic
                        vcov = NeweyWest(unres_model, lag = 7, prewhite = F))$F[2]
}

# QLR test statistic
max(Fstats)
####----------------------------------------------------------------------------------