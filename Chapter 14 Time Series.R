#### Use regression models for forecasting #### 
library(AER) 
library(dynlm) 
library(forecast) 
library(readxl) 
library(stargazer) 
library(scales) 
library(quantmod)    # to plot time series 
library(urca)

data("CASchools") 

# 1. Create new variables 
CASchools$STR <- CASchools$students / CASchools$teachers    # student-teacher ratio 
CASchools$score <- (CASchools$read + CASchools$math) / 2    # average test score

# 2. Linear Regression Model 
model <- lm(score ~ STR, data = CASchools)    
model       

predict(model, newdata = data.frame("STR" = 25))    # predict score when STR=25
####----------------------------------------------------------------------------------

#### Time series data and serial correlation #### 
# 1. Plot time series data
USMacroSWQ <- read_xlsx("/Users/nicoleyin88/Documents/Other /1. R/1. R Tutorial/us_macro_quarterly.xlsx", 
                        sheet = 1,
                        col_types = c("text", rep("numeric", 9)))   # 1st col is text, the rest 9 cols are numeric

USMacroSWQ$...1 <- as.yearqtr(USMacroSWQ$...1, format = "%Y:0%q")     # format data col

colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", 
                          "PCECTPI", "GS10", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")    # adjust col names 

GDP <- xts(USMacroSWQ$GDPC96, USMacroSWQ$Date)["1960::2013"]    # GDP series as xts object 

GDPGrowth <- xts(400 * log(GDP / (stats::lag(GDP)-1)))    # GDP growth series as xts object 

plot(log(as.zoo(GDP)),        
     col = "blue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date", 
     main = "U.S. Quarterly Real GDP")

plot(as.zoo(GDPGrwoth),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date", 
     main = "U.S. Real GDP Growth Rate")


# (2) Notation, lags, differences, log, growth rates 
quants <- function(series) {       
  s <- series
  return(
    data.frame("level" = s,      # GDP in levels 
               "Logarithm" = log(s),    # GDP in logs
               "AnnualGrowthRate" = 400 * log(s / lag(s)),      # GDP in %
               "1stLagAnnualGrwothRate" = lag(400 * log(s / lag(s))))    # 1st difference in %GDP
  )
}

quants(GDP["2011-07::2013-01"])    # Extract data 

acf(na.omit(GDPGrwoth), lag.max = 4, plot = F)    # autocorrelation function


# (3) other examples of time series 
USUnemp <- xts(USMacroSWQ$UNRATE, USMacroSWQ$Date)["1960::2013"]    # define series as xts objects, where xts() is for raw data.

DollarPoundFX <- xts(USMacroSWQ$EXUSUK, USMacroSWQ$Date)["1960::2013"]

JPIndProd <- xts(log(USMacroSWQ$JAPAN_IP), USMacroSWQ$Date)["1960::2013"]

data("NYSESW")     # attach NYSESW data
NYSESW <- xts(Delt(NYSESW))

par(mfrow = c(2,2))

plot(as.zoo(USUnemp),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent",
     xlab = "Date",
     main = "US Enemployment Rate",
     cex.main = 1)

plot(as.zoo(DollarPoundFX),
     col = "steelblue",
     lwd = 2,
     ylab = "Dollar per pound",
     xlab = "Date",
     main = "U.S. Dollar / B. Pound Exchange Rate",
     cex.main = 1)

plot(as.zoo(JPIndProd),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "Japanese Industrial Production",
     cex.main = 1)

plot(as.zoo(NYSESW),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent per Day",
     xlab = "Date",
     main = "New York Stock Exchange Composite Index",
     cex.main = 1)

acf(na.omit(NYSESW), plot = F, lag.max = 10)

acf(na.omit(NYSESW), main = "Sample autocorrelation for NYSESW Data", ylim = c(-0.1, 0.1))
####----------------------------------------------------------------------------------

#### Autoregressions #### 
# subset data
GDPGRSub <- GDPGrowth["1962::2012"]

# estimate the model
ar.ols(GDPGRSub, order.max = 1,
       demean = F,
       intercept = T)

# length of data set
N <- length(GDPGRSub)

GDPGR_level <- as.numeric(GDPGRSub[-1])
GDPGR_lags <- as.numeric(GDPGRSub[-N])

# estimate the model
armod <- lm(GDPGR_level ~ GDPGR_lags) 
armod

coeftest(armod, vcov. = vcovHC, type = "HC1")

# assign GDP growth rate in 2012:Q4
new <- data.frame("GDPGR_lags" = GDPGR_level[N-1])

# forecast GDP growth rate in 2013:Q1
forecast(armod, newdata = new)

# compute the forecast error
forecast(armod, newdata = new)$mean - GDPGrowth["2013"][1]

# R^2
summary(armod)$r.squared

# SER
summary(armod)$sigma

# estimate the AR(2) model
GDPGR_AR2 <- dynlm(ts(GDPGR_level) ~ L(ts(GDPGR_level)) + L(ts(GDPGR_level), 2)) 
coeftest(GDPGR_AR2, vcov. = sandwich)

# R^2
summary(GDPGR_AR2)$r.squared

# SER
summary(GDPGR_AR2)$sigma

# AR(2) forecast of GDP growth in 2013:Q1
forecast <- c("2013:Q1" = coef(GDPGR_AR2) %*% c(1, GDPGR_level[N-1], GDPGR_level[N-2]))

# compute AR(2) forecast error
GDPGrowth["2013"][1] - forecast
####----------------------------------------------------------------------------------

#### Can you beat the market #### 
# read in data on stock returns
SReturns <- read_xlsx("/Users/nicoleyin88/Documents/Other /1. R/1. R Tutorial/Stock_Returns_1931_2002.xlsx", 
                      sheet = 1,
                      col_types = "numeric")
# convert to ts object
StockReturns <- ts(SReturns[, 3:4], start = c(1931, 1), end = c(2002, 12), frequency = 12)

# estimate AR models:
# AR(1)
SR_AR1 <- dynlm(ExReturn ~ L(ExReturn),
                data = StockReturns, start = c(1960, 1), end = c(2002, 12))

# AR(2)
SR_AR2 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 2),
                data = StockReturns, start = c(1960, 1), end = c(2002, 12))

# AR(4)
SR_AR4 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 1:4),
                data = StockReturns, start = c(1960, 1), end = c(2002, 12))

rob_se <- list(sqrt(diag(sandwich(SR_AR1))), 
               sqrt(diag(sandwich(SR_AR2))), 
               sqrt(diag(sandwich(SR_AR4))))

stargazer(SR_AR1, SR_AR2, SR_AR4,
          title = "Autoregressive Models of Monthly Excess Stock Returns",
          header = FALSE,
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3,
          column.labels = c("AR(1)", "AR(2)", "AR(4)"),
          dep.var.caption = "Dependent Variable: Excess Returns on the CSRP Value-Weighted Index", 
          dep.var.labels.include = FALSE,
          covariate.labels = c("$excess return_{t-1}$", "$excess return_{t-2}$",
                               "$excess return_{t-3}$", "$excess return_{t-4}$",
                               "Intercept"),
                               se = rob_se,
                               omit.stat = "rsq")
####----------------------------------------------------------------------------------

#### Additional Predictors and The ADL Model #### 
# 3-months Treasury bills interest rate
TB3MS <- xts(USMacroSWQ$TB3MS, USMacroSWQ$Date)["1960::2012"] 

# 10-years Treasury bonds interest rate
TB10YS <- xts(USMacroSWQ$GS10, USMacroSWQ$Date)["1960::2012"]

# term spread
TSpread <- TB10YS - TB3MS

# reproduce Figure 14.2 (a) of the book
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)), plot.type = "single",
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Interest Rates")

# define function that transform years to class 'yearqtr'
YToYQTR <- function(years) { return(
  sort(as.yearqtr(sapply(years, paste, c("Q1", "Q2", "Q3", "Q4")))) )
}

# recessions
recessions <- YToYQTR(c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991, 2001, 2007:2008))

# add color shading for recessions
xblocks(time(as.zoo(TB3MS)), 
        c(time(TB3MS) %in% recessions),
        col = alpha("steelblue", alpha = 0.3))

# add a legend
legend("topright",legend = c("TB3MS", "TB10YS"), 
       col = c("darkred", "steelblue"), lwd = c(2, 2))

# reproduce Figure 14.2 (b) of the book
plot(as.zoo(TSpread), col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Term Spread")

# add color shading for recessions
xblocks(time(as.zoo(TB3MS)), c(time(TB3MS) %in% recessions),
        col = alpha("steelblue", alpha = 0.3))

# convert growth and spread series to ts objects
GDPGrowth_ts <- ts(GDPGrowth,
                   start = c(1960, 1),
                   end = c(2013, 4), frequency = 4)

TSpread_ts <- ts(TSpread,
                 start = c(1960, 1),
                 end = c(2012, 4), frequency = 4)

# join both ts objects
ADLdata <- ts.union(GDPGrowth_ts, TSpread_ts)

# estimate the ADL(2,1) model of GDP growth
GDPGR_ADL21 <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) + L(TSpread_ts), start = c(1962, 1), end = c(2012, 4))

coeftest(GDPGR_ADL21, vcov. = sandwich)

# 2012:Q3 / 2012:Q4 data on GDP growth and term spread
subset <- window(ADLdata, c(2012, 3), c(2012, 4))

# ADL(2,1) GDP growth forecast for 2013:Q1
ADL21_forecast <- coef(GDPGR_ADL21) %*% c(1, subset[2, 1], subset[1, 1], subset[2, 2]) 
ADL21_forecast

# compute the forecast error
window(GDPGrowth_ts, c(2013, 1), c(2013, 1)) - ADL21_forecast

# estimate the ADL(2,2) model of GDP growth
GDPGR_ADL22 <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) + L(TSpread_ts) + L(TSpread_ts, 2),
                     start = c(1962, 1), end = c(2012, 4))

coeftest(GDPGR_ADL22, vcov. = sandwich)

# ADL(2,2) GDP growth forecast for 2013:Q1
ADL22_forecast <- coef(GDPGR_ADL22) %*% c(1, subset[2, 1], subset[1, 1], subset[2, 2], subset[1,2]) 
ADL22_forecast

# compute the forecast error
window(GDPGrowth_ts, c(2013, 1), c(2013, 1)) - ADL22_forecast

# compare adj. R2
c("Adj.R2 AR(2)" = summary(GDPGR_AR2)$r.squared, 
  "Adj.R2 ADL(2,1)" = summary(GDPGR_ADL21)$r.squared, 
  "Adj.R2 ADL(2,2)" = summary(GDPGR_ADL22)$r.squared)

# compare SER
c("SER AR(2)" = summary(GDPGR_AR2)$sigma, 
  "SER ADL(2,1)" = summary(GDPGR_ADL21)$sigma, 
  "SER ADL(2,2)" = summary(GDPGR_ADL22)$sigma)

# F-test on coefficients of term spread
linearHypothesis(GDPGR_ADL22,
                 c("L(TSpread_ts)=0", "L(TSpread_ts, 2)=0"),
                 vcov. = sandwich)
####----------------------------------------------------------------------------------

#### Forecast Uncertainty and Forecast Intervals #### 
# set seed
set.seed(1234)

# simulate the time series
Y <- arima.sim(list(order = c(2, 0, 0), ar = c(0.2, 0.2)), n = 200) # estimate an AR(2) model using 'arima()'

model <- arima(Y, order = c(2, 0, 0))

# compute points forecasts and prediction intervals for the next 25 periods
fc <- forecast(model, h = 25, level = seq(5, 99, 10))

# plot a fan chart
plot(fc,
     main = "Forecast Fan Chart for AR(2) Model of Simulated Data", 
     showgap = F,
     fcol = "red",
     flty = 2)
####----------------------------------------------------------------------------------

#### Lag Length Selection Using Information Criteria #### 
# compute BIC for AR model objects of class 'dynlm'
BIC <- function(model) {
  ssr <- sum(model$residuals^2) 
  t <- length(model$residuals) 
  npar <- length(model$coef)
  return(round(c("p" = npar - 1, 
            "BIC" = log(ssr/t) + npar * log(t)/t, 
            "R2" = summary(model)$r.squared), 4)) 
  }

# apply the BIC() to an intercept-only model of GDP growth
BIC(dynlm(ts(GDPGR_level) ~ 1))

# loop BIC over models of different orders
order <- 1:6

BICs <- sapply(order, function(x)
  "AR" = BIC(dynlm(ts(GDPGR_level) ~ L(ts(GDPGR_level), 1:x))))

#  select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])]

# loop 'BIC()' over multiple ADL models
order <- 1:12

BICs <- sapply(order, function(x)
  BIC(dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts, 1:x) + L(TSpread_ts, 1:x),
            start = c(1962, 1), end = c(2012, 4))))
BICs

# select the ADL model with the smallest BIC
BICs[, which.min(BICs[2, ])]
####----------------------------------------------------------------------------------

#### Nonstationarity I: Trends #### 
# simulate and plot random walks starting at 0
set.seed(1)

RWs <- ts(replicate(n = 4,
                    arima.sim(model = list(order = c(0, 1 ,0)), n = 100)))
matplot(RWs,
        type = "l",
        col = c("steelblue", "darkgreen", "darkred", "orange"), lty = 1,
        lwd = 2,
        main = "Four Random Walks",
        xlab = "Time",
        ylab = "Value")

#  simulate and plot random walks with drift
set.seed(1)

RWsd <- ts(replicate(n = 4,
                     arima.sim(model = list(order = c(0, 1, 0)),
                               n = 100,
                               mean = -0.2)))

matplot(RWsd,
        type = "l",
        col = c("steelblue", "darkgreen", "darkred", "orange"), lty = 1,
        lwd = 2,
        main = "Four Random Walks with Drift", xlab = "Time",
        ylab = "Value")

# plot spurious relationship
matplot(RWs[, c(2, 3)], lty = 1,
        lwd = 2,
        type = "l",
        col = c("darkgreen", "darkred"), xlab = "Time",
        ylab = "",
        main = "A Spurious Relationship")

# estimate spurious AR model
summary(dynlm(RWs[, 2] ~ L(RWs[, 3])))$coefficients

# plot U.S. unemployment rate & Japanese industrial production
plot(merge(as.zoo(USUnemp), as.zoo(JPIndProd)), plot.type = "single",
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "",
     main = "Spurious Regression: Macroeconomic Time series")

# add a legend
legend("topleft",
       legend = c("USUnemp", "JPIndProd"), 
       col = c("darkred", "steelblue"), lwd = c(2, 2))

# estimate regression using data from 1962 to 1985
SR_Unemp1 <- dynlm(ts(USUnemp["1962::1985"]) ~ ts(JPIndProd["1962::1985"])) 

coeftest(SR_Unemp1, vcov = sandwich)

# Estimate regression using data from 1986 to 2012
SR_Unemp2 <- dynlm(ts(USUnemp["1986::2012"]) ~ ts(JPIndProd["1986::2012"])) 

coeftest(SR_Unemp2, vcov = sandwich)

# repetitions
N <- 1000

# observations
n <- 1000

# define constant, trend and rho
drift <- 0.5 
trend <- 1:n 
rho <- 1

# function which simulates an AR(1) process
AR1 <- function(rho) { 
  out <- numeric(n) 
  for(i in 2:n) {
    out[i] <- rho * out[i-1] + rnorm(1) 
  }
  return(out) 
}

# simulate from DGP with constant
RWD <- ts(replicate(n = N, drift + AR1(rho)))

# compute ADF test statistics and store them in 'ADFD'
ADFD <- numeric(N)

for(i in 1:ncol(RWD)) { 
  ADFD[i] <- summary(
  dynlm(diff(RWD[, i], 1) ~ L(RWD[, i], 1)))$coef[2, 3] 
}

# simulate from DGP with constant and trend
RWDT <- ts(replicate(n = N, drift + trend + AR1(rho))) # compute ADF test statistics and store them in 'ADFDT'

ADFDT <- numeric(N)

for(i in 1:ncol(RWDT)) { 
  ADFDT[i] <- summary(
  dynlm(diff(RWDT[, i], 1) ~ L(RWDT[, i], 1) + trend(RWDT[, i])) )$coef[2, 3]
}

# estimate quantiles for ADF regression with a drift
round(quantile(ADFD, c(0.1, 0.05, 0.01)), 2)

# estimate quantiles for ADF regression with drift and trend
round(quantile(ADFDT, c(0.1, 0.05, 0.01)), 2)

# plot standard normal density
curve(dnorm(x),
      from = -6, to = 3,
      ylim = c(0, 0.6),
      lty = 2,
      ylab = "Density",
      xlab = "t-Statistic",
      main = "Distributions of ADF Test Statistics", col = "darkred",
      lwd = 2)

# plot density estimates of both Dickey-Fuller distributions
lines(density(ADFD), lwd = 2, col = "darkgreen") 
lines(density(ADFDT), lwd = 2, col = "blue")

# add a legend
legend("topleft",
       c("N(0,1)", "Drift", "Drift+Trend"), 
       col = c("darkred", "darkgreen", "blue"), lty = c(2, 1, 1),
       lwd = 2)

# generate log GDP series
LogGDP <- ts(log(GDP["1962::2012"]))

# estimate the model
coeftest(dynlm(diff(LogGDP) ~ trend(LogGDP, scale = F) + L(LogGDP)
        + diff(L(LogGDP)) + diff(L(LogGDP), 2)))

# test for unit root in GDP using 'ur.df()' from the package 'urca'
summary(ur.df(LogGDP,
              type = "trend",
              lags = 2,
              selectlags = "Fixed"))
####----------------------------------------------------------------------------------

#### Nonstationarity II: Breaks #### 
# set up a range of possible break dates
tau <- seq(1970, 2005, 0.25)

# initialize vector of F-statistics
Fstats <- numeric(length(tau))

# estimation loop over break dates
for(i in 1:length(tau)) {             # set up dummy variable
  D <- time(GDPGrowth_ts) > tau[i]     
  test <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) 
                + L(GDPGrowth_ts, 2) + D*L(TSpread_ts)    # estimate ADL(2,2) model with intercations
                + D*L(TSpread_ts, 2),
                start = c(1962, 1), end = c(2012, 4))
  Fstats[i] <- linearHypothesis(test,                     # compute and save the F-statistic
                                c("DTRUE=0", "DTRUE:L(TSpread_ts)",
                                  "DTRUE:L(TSpread_ts, 2)"), 
                                vcov. = sandwich)$F[2]
}

# identify QLR statistic
QLR <- max(Fstats) 
QLR

# identify the time period where the QLR-statistic is observed
as.yearqtr(tau[which.max(Fstats)])

# series of F-statistics
Fstatsseries <- ts(Fstats,
                   start = tau[1],
                   end = tau[length(tau)], frequency = 4)

# plot the F-statistics
plot(Fstatsseries,
     xlim = c(1960, 2015),
     ylim = c(1, 7.5),
     lwd = 2,
     col = "steelblue",
     ylab = "F-Statistic",
     xlab = "Break Date",
     main = "Testing for a Break in GDP ADL(2,2) Regression at Different Dates")

# dashed horizontal lines for critical values and QLR statistic
abline(h = 4.71, lty = 2)
abline(h = 6.02, lty = 2)

segments(0, QLR, 1980.75, QLR, col = "darkred") 

text(2010, 6.2, "1% Critical Value") 
text(2010, 4.9, "5% Critical Value") 
text(1980.75, QLR+0.2, "QLR Statistic")

# end of sample dates
EndOfSample <- seq(2002.75, 2012.5, 0.25)

# initialize matrix forecasts
forecasts <- matrix(nrow = 1,
                    ncol = length(EndOfSample))

# initialize vector SER
SER <- numeric(length(EndOfSample))

# estimation loop over end of sample dates
for(i in 1:length(EndOfSample)) {
  m <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) 
             + L(GDPGrowth_ts, 2) + L(TSpread_ts) 
             + L(TSpread_ts, 2),                  # estimate ADL(2,2) model
             start = c(1981, 1), end = EndOfSample[i])
  SER[i] <- summary(m)$sigma
  s <- window(ADLdata, EndOfSample[i] 
              - 0.25, EndOfSample[i])      # sample data for one-period ahead forecast
  forecasts[i] <- coef(m) %*% c(1, s[1, 1], 
                                s[2, 1], s[1, 2], s[2, 2])    # compute forecast
}

# compute psuedo-out-of-sample forecast errors
POOSFCE <- c(window(GDPGrowth_ts, c(2003, 1), c(2012, 4))) - forecasts

# series of pseudo-out-of-sample forecasts
PSOSSFc <- ts(c(forecasts), start = 2003,
              end = 2012.75,
              frequency = 4)

# plot the GDP growth time series
plot(window(GDPGrowth_ts, c(2003, 1), c(2012, 4)), col = "steelblue",
     lwd = 2,
     ylab = "Percent",
     main = "Pseudo-Out-Of-Sample Forecasts of GDP Growth")

# add the series of pseudo-out-of-sample forecasts
lines(PSOSSFc, lwd = 2, lty = 2)

# shade area between curves (the pseudo forecast error)
polygon(c(time(PSOSSFc), rev(time(PSOSSFc))),
        c(window(GDPGrowth_ts, c(2003, 1), c(2012, 4)), 
          rev(PSOSSFc)), col = alpha("blue", alpha = 0.3),
        border = NA)

# add a legend
legend("bottomleft", lty = c(1, 2, 1),
       lwd = c(2, 2, 10),
       col = c("steelblue", "black", alpha("blue", alpha = 0.3)), 
       legend = c("Actual GDP growth rate",
                  "Forecasted GDP growth rate",
                  "Pseudo forecast Error"))

# SER of ADL(2,2) mode using data from 1981:Q1 - 2002:Q4
SER[1]

# compute root mean squared forecast error
sd(POOSFCE)

# test if mean forecast error is zero
t.test(POOSFCE)
####----------------------------------------------------------------------------------

#### Can You Beat the Market? (Part II) #### 
# plot logarithm of dividend yield series
plot(StockReturns[, 2], col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     main = "Dividend Yield for CRSP Index")

# test for unit root in GDP using 'ur.df()' from the package 'urca'
summary(ur.df(window(StockReturns[, 2], c(1960,1),
                     c(2002, 12)), type = "drift",
              lags = 0))

# ADL(1,1) (1st difference of log dividend yield)
CRSP_ADL_1 <- dynlm(ExReturn ~ L(ExReturn) + d(L(ln_DivYield)), data = StockReturns,
                    start = c(1960, 1), end = c(2002, 12))

# ADL(2,2) (1st & 2nd differences of log dividend yield)
CRSP_ADL_2 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 2)
                    + d(L(ln_DivYield)) + d(L(ln_DivYield, 2)),
                    data = StockReturns,
                    start = c(1960, 1), end = c(2002, 12))

# ADL(1,1) (level of log dividend yield)
CRSP_ADL_3 <- dynlm(ExReturn ~ L(ExReturn) + L(ln_DivYield), data = StockReturns,
                    start = c(1960, 1), end = c(1992, 12))

# gather robust standard errors
rob_se_CRSP_ADL <- list(sqrt(diag(sandwich(CRSP_ADL_1))), 
                        sqrt(diag(sandwich(CRSP_ADL_2))), 
                        sqrt(diag(sandwich(CRSP_ADL_3))))

stargazer(CRSP_ADL_1, CRSP_ADL_2, CRSP_ADL_3,
          title = "ADL Models of Monthly Excess Stock Returns",
          header = FALSE,
          type = "text",
          column.sep.width = "-5pt",
          no.space = F,
          digits = 3,
          column.labels = c("ADL(1,1)", "ADL(2,2)", "ADL(1,1)"),
          dep.var.caption = "Dependent Variable: Excess returns on the CSRP value-weighted index", 
          dep.var.labels.include = FALSE,
          covariate.labels = c("$excess return_{t-1}$",
                               "$excess return_{t-2}$",
                               "$1^{st} diff log(dividend yield_{t-1})$",
                               "$1^{st} diff log(dividend yield_{t-2})$",
                               "$log(dividend yield_{t-1})$",
                               "Constant"),
          se = rob_se_CRSP_ADL)

# end of sample dates
EndOfSample <- as.numeric(window(time(StockReturns), 
                                 c(1992, 12), c(2002, 11)))

# initialize matrix  forecasts
forecasts <- matrix(nrow = 2,
                    ncol = length(EndOfSample))

# estimation loop over end of sample dates
for(i in 1:length(EndOfSample)) {
  mod3 <- dynlm(ExReturn ~ L(ExReturn) + L(ln_DivYield),    # estimate model (3)
                data = StockReturns, start = c(1960, 1),
                end = EndOfSample[i])
  modconst <- dynlm(ExReturn ~ 1,       # estimate intercept only model
                    data = StockReturns, start = c(1960, 1),
                    end = EndOfSample[i])
  t <- window(StockReturns,      # sample data for one-period ahead forecast
              EndOfSample[i], 
              EndOfSample[i])     
  forecasts[, i] <- c(coef(mod3) %*% c(1, t[1], t[2]),   # compute forecast
                      coef(modconst)) 
  }

# gather data
d <- cbind("Excess Returns" = c(window(StockReturns[,1], 
                                       c(1993, 1), c(2002, 12))), "Model (3)" = forecasts[1,],
           "Intercept Only" = forecasts[2,],
           "Always Zero" =  0)

# Compute RMSFEs
c("ADL model (3)" = sd(d[, 1] - d[, 2]), 
  "Intercept-only model" = sd(d[, 1] - d[, 3]), 
  "Always zero" = sd(d[,1] - d[, 4]))
####----------------------------------------------------------------------------------