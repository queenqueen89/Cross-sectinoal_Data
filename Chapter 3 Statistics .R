#### Estimation of population mean ####
# (1) plot Chi-squared with df=12
curve(dchisq(x, df = 12),            
      from = 0,
      to = 40,
      ylab = "density",
      xlab = "hourley earnings in Euro",
      col = "deeppink2",
      lwd = 2,
      main = "Chi-Sqaured Distribution with df=12")


# (2) sample from the Chi-squared with df=12
rsamp <- rchisq(n = 100, df = 12)    
rsamp[1]                              # use only the 1st observation
####----------------------------------------------------------------------------------

#### Properties of sample mean #### 
# (1) Plot Sampling Distributions of unbiased estimators 
pop <- rnorm(10000, 10, 1)           # generate a fictious population, pop size=10000, mean=10, sd=1

est1 <- replicate(expr = mean(sample(x = pop, size = 5)), n = 25000)     # sample from pop and estimate the mean
est2 <- replicate(expr = mean(sample(x = pop, size = 25)), n = 25000)
fo <- replicate(expr = sample(x = pop, size = 5)[1], n = 25000)

is.vector(est1)          # check if est1 is vector and length
is.vector(est2)
length(est1)
length(est2)

plot(density(fo),        # plot density estimate Y1 (fo)
     col = "deeppink2",
     lwd = 2,
     ylim = c(0,2),
     xlab = "estimates",
     main = "Sampling Distributons of Unbiased estimators")

lines(density(est1),     # density estimate for sample mean when n=5
      col = "gold",
      lwd = 2,
      bty = "l")

lines(density(est2),     # density estimate for sample mean when n=25
      col = "blue",
      lwd = 2)

abline(v = 10, lty = 2)  # add vertical line at true mean, v=10 (true mean)

curve(dnorm(x, mean = 10),   # add N(10,1) density
      lwd = 2,
      lty = 2,
      add = T)

legend("topleft",
       legend = c("N(10,1)",
                  expression(Y[1]),
                  expression(bar(Y) ~ n == 5),
                  expression(bar(Y) ~ n == 25)
                  ),
       lty = c(2,1,1,1),
       col = c('black', 'deeppink2', 'gold', 'blue'),
       lwd = 2)

# (2) Show bar(Y) is the least squares estimator of mu(Y)
sqm <- function(m) {      # define function m: sum[(y-m)^2]
  sum((y-m)^2)            
}
sqm <- Vectorize(sqm)     # vectorize function m
y <- rnorm(100, 10, 1)    # draw random sample and find the mean
mean(y)

curve(sqm(x),             # plot function m
      from = -50,
      to = 70,
      xlab = "m",
      ylab = "sqm(m)",
      col = "deeppink2")

abline(v = mean(y),        # draw a vertical line at mean(y) 
       lty = 2)

text(x = mean(y),          # annotate at mean(y) = 10.16
     lty = 2,
     labels = paste(round(mean(y),2)))    # label mean, round to 2 decimals


# (3) Why random sampling is important (need iid)
mean(pop)   # find pop mean for the fictious pop

est3 <- replicate(n = 25000,        # simulate outsomes for the sample mean when iid fails
                  expr = mean(sample(x = sort(pop),
                                     size = 10,
                                     prob = c(rep(4,2500), rep(1, 7500)))))
mean(est3)            # find sample mean of the outcomes

plot(density(est2),    # plot sampling distribution of sample mean, iid holds for n=25
     col = "deeppink2",
     lwd = 2,
     xlim = c(8,11),
     xlab = "Estimates",
     main = "When the iid Assumption Fails")

lines(density(est3),   # plot sampling distribution of sample mean, iid fails for n=25
      col = "gold",
      lwd = 2)

legend("topleft",
       legend = c(expression(bar(Y)[n == 25]~",iid fails"),
                  expression(bar(Y)[n == 25]~",iid holds")
                  ),
       lty = c(1,1),
       col = c("gold","deeppink2"),
       lwd = 2)
####----------------------------------------------------------------------------------

#### Hypothesis tests concerning the population mean #### 
# (1) Plot p-value when sd is known
curve(dnorm(x),           # plot standard normal density on interval [-4,4]
      xlim = c(-4,4),
      main = "Calculating a p-value",
      yaxs = "i",
      xlab = "z",
      ylab = "",
      lwd = 2,
      axes = "F")

axis(1,                          # add x-axis 
     at = c(-1.5,0,1.5),
     padj = 0.75,
     labels = c(expression(-frac(bar(Y)^"act"~-bar(mu)[Y,0], sigma[bar(Y)])),
                0,
                expression(frac(bar(Y)^"act"~-bar(mu)[Y,0], sigma[bar(Y)]))))

polygon(x = c(-6, seq(-6, -1.5, 0.01), -1.5),      # use polygon() to add shade p-value: region in left tail
        y = c(0, dnorm(seq(-6, -1.5, 0.01)), 0),
        col = "deeppink2")

polygon(x = c(1.5, seq(1.5, 6, 0.01), 6),         # shade region in left tail
        y = c(0, dnorm(seq(1.5, 6, 0.01)), 0),
        col = "deeppink2")

# (2) Plot sampling distribution of S(Y)
n <- c(10000, 5000, 2000, 1000, 500)      # Make a vector of sample sizes

sq_y <- replicate(n = 10000, expr = sd(rnorm(n[1], 10, 10)))     # sample obervations, estimte with sd(), use the first sample rnorm(n[1])
plot(density(sq_y),                                              # plot sampling distributions for n=10000
     main = expression("Sampling Distributions of" ~ s[Y]),
     xlab = expression(s[Y]),
     lwd = 2)

for (i in 2:length(n)) {                                         # use for loop to plot for i in 2:length(n) where n=5000, 2000, 1000, 500
  sq_y <- replicate(n = 10000, expr = sd(rnorm(n[i],10,10)))
  lines(density(sq_y),
        col = i,
        lwd = 2)
}

legend("topleft",
       legend = c(expression(n == 10000),
                  expression(n == 5000),
                  expression(n == 2000),
                  expression(n == 1000),
                  expression(n == 500)),
       col = 1:5,
       lwd = 2)

# (3) check if sample mean and sample SE are close to true value 
mean_est <- numeric(10000)
se_est <- numeric(10000)

for (i in 1:10000) {
  s <- sample(0:1,
              size = 100,
              prob = c(0.9,0.1),
              replace = T)
  mean_est[i] <- mean(s)
  se_est[i] <- sqrt(mean(s)*(1-mean(s))/100)
}

mean(mean_est)

# (4) find p-value when sd is unknown 
samplemean_act <- mean(               # find SE
  sample(0:1,
         prob = c(0.9,0.1),
         replace = T,
         size = 100))

SE_samplemean <- sqrt(samplemean_act * (1 - samplemean_act)/100)

mean_h0 <- 0.1       # h0 hypothesis: suppose true mean is 0.1
pvalue <- 2* pnorm(-abs(samplemean_act - mean_h0) / SE_samplemean)     # calcuate p-value 
pvalue 

# (5) t-stasistic
tstat <- (samplemean_act - mean_h0) / SE_samplemean     # calculate t-stat
tstat

tstat <- numeric(10000)
n <- 300
for (i in 1:10000) {
  
  s <- sample(0:1,
              size = n,
              prob = c(0.9, 0.1),
              replace = T)
  
  tstat[i] <- (mean(s) - 0.1)/sqrt(var(s)/n)
}

plot(density(tstat),
     xlab = 't-statistic',
     main = 'Estimated Distribution of the t-statistic when n=300',
     lwd = 2, 
     xlim = c(-4,4),
     col = 'deeppink2')

curve(dnorm(x),
      add = T,
      lty = 2,
      lwd = 2)

# (6) Hypo testing with a specified sig level 
pvalue < 0.05

qnorm(p = 0.975)

abs(tstat) > 1.96

# (7) Right-sided test 
curve(dnorm(x),     # plot standard normal density on domain [-4,4]
      xlim = c(-4,4),
      main = "Rejection region of a right-sided test",
      yaxs = "i",
      xlab = "t-statistic",
      lwd = 2,
      axes = "F")

axis(1,             # add x-axis
     at = c(-4, 0, 1.64, 4),
     padj = 0.5,
     labels = c('', 0, expression(Phi^-1~(.95)==1.64),''))

polygon(x = c(1.64, seq(1.64, 4, 0.01), 4),
        y = c(0, dnorm(seq(1.64, 4, 0.01)), 0),
        col = "deeppink2")

# (8) Left-sided test
curve(dnorm(x),          # plot standard normal density on domain [-4,4]
      xlim = c(-4,4),
      main = "Rejection region of a left-sided test",
      yaxs = "i",
      xlab = "t-statistic",
      lwd = 2,
      axes = "F")

axis(1,             # add x-axis
     at = c(-4, -1.64, 0, 4),
     padj = 0.5,
     labels = c('', expression(Phi^-1~(0.05)==-1.64),0, ''))

polygon(x = c(-4, seq(-4, -1.64, 0.01), -1.64),
        y = c(0, dnorm(seq(-4, -1.64, 0.01)), 0),
        col = "deeppink2")
####----------------------------------------------------------------------------------

#### CI for population mean #### 
set.seed(1)

sampledata <- rnorm(100, 10, 10)

typeof(t.test(sampledata))     # check type of the outcome produced by t.test

ls(t.test(sampledata))         # list elements produced by t.test

t.test(sampledata)$conf.int    # extract CI from t.test

t.test(sampledata)             # show all output from t.test

p_value <- t.test(sampledata)$p.value    # test if p-value < 0.05
p_value
if (p_value < 0.05) {
  print("Reject H0")
}
####----------------------------------------------------------------------------------

#### Compare means from different populations #### 
set.seed(1)

sample_pop1 <- rnorm(100, 10, 10)
sample_pop2 <- rnorm(100, 10, 20)
t.test(sample_pop1, sample_pop2)
####----------------------------------------------------------------------------------

#### Application: gender gap earnings #### 
# load data 
setwd("/Users/nicoleyin88/Documents/Other /Programming/R/* R Tutorial/")
cps <- read.csv("cps.csv")    

library(dplyr)

head(cps)     # get overview of data structure

avgs <- cps %>%           
        group_by(a_sex, year) %>%      # group data by gender & year
        summarise(mean(ahe08),         # find mean of each group
                  sd(ahe08),           # find sd of each group 
                  n())                 # add number for each group 
print(avgs)

male <- avgs %>% filter(a_sex == 1)   # split the dataset by gender
female <- avgs %>% filter(a_sex == 2)

colnames(male) <- c("Sex", "Year", "Y_bar_m", "s_m", "n_m")   # rename columns of both splits 
colnames(female) <- c("Sex", "Year", "Y_bar_f", "s_f", "n_f")

gap <- male$Y_bar_m - female$Y_bar_f      # find wage gap between male and female 
gap_se <- sqrt(male$s_m^2 / male$n_m + female$s_f^2 / female$n_f)    # find se of wage gap 

gap_ci_l <- gap - 1.96 * gap_se     # find lower CI
gap_ci_u <- gap + 1.96 * gap_se     # find upper CI

result <- cbind(male[,-1], female[,-(1:2)], gap, gap_se, gap_ci_l, gap_ci_u)     
print(result, digits = 3)
####----------------------------------------------------------------------------------

#### Scatterplots, sample covariance, sample correlation #### 
# (1) Sacatterplot 
set.seed(123)

X <- runif(n = 100,       # runif() generates random deviates for uniform distribution 
           min = 18,
           max = 70)

Y <- X + rnorm(n = 100, 50, 15)

plot(X,
     Y,
     type = "p",
     main = "A scatterplot of X and Y",
     xlab = "age",
     ylab = "earnings",
     col = "blue",
     pch = 19)

# (2) sample covariance and correlation
cov(X, Y)    # find covariance
cor(X,Y)     # find correlation

# (3) Plot correlation
set.seed(1)
ex1 <- mvrnorm(100,       # generate bivariate sample data with different degrees of correlation
               mu = c(0, 0),
               Sigma = matrix(c(2,2,2,3), ncol = 2),
               empirical = TRUE) 
ex2 <- mvrnorm(100,
               mu = c(0,0),
               Sigma = matrix(c(2,-2,-2,3),ncol = 2),
               empirical = T)
ex3 <- mvrnorm(100,
               mu = c(0,0),
               Sigma = matrix(c(1,0,0,1),ncol = 2),
               empirical = T)

X <- seq(-3,3,0.01)
Y <- -X^2 + rnorm(length(X))
ex4 <- cbind(X,Y)

par(mfrow = c(2,2))

plot(ex1, col = "blue", pch = 20, xlab = "X", ylab = "Y",
     main = "Correlation = 0.81")

plot(ex2, col = "blue", pch = 20, xlab = "X", ylab = "Y",
     main = "Correlation = -0.81")

plot(ex3, col = "blue", pch = 20, xlab = "X", ylab = "Y",
     main = "Correlation = 0")

plot(ex4, col = "blue", pch = 20, xlab = "X", ylab = "Y",
     main = "Correlation = 0")
####----------------------------------------------------------------------------------