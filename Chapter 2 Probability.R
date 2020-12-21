#### Discrete Random Variables ####
sample(1:6, 1)                # (1) roll a dice: randomly draw a number from 1 thru 6
probability <- rep(1/6, 6)    # (2) make a vector of probabilities (with prob.1/6 for 6 times)
plot(probability,             # (3) plot the probability (this first line gives default y-axis name"probability")
     main = "Probability Distribution",   # (4) set title "Probability Distribution"
     xlab = "Outcomes")       # (5) lable x-axis "Outcomes", y-axis is labeled "probability" by default. 

sample(1:6, 1)                
probability <- rep(1/6, 6)  
cum_probability <- cumsum(probability)    # (6) make a vector of cumulative probabilities
plot(cum_probability,                     # get default y-axis lable "cum_probability"     
     main = "Cumulative Probability Distribution",  
     xlab = "Outcomes",
     ylab = "")                           # Use ylab="" to get rid of the y-lable. 
####----------------------------------------------------------------------------------

#### Bernoulli Trials #### 
sample(c("H","T"), 10,replace = TRUE)   # (1) simulate tossing a coin 10 times. Need "replace = True" in this case.
# It is {"H" "H" "T" "H" "H" "H" "H" "T" "T" "H"}

sample(c("H","T"), 1)  # (2) simulate tossing a coin once, get H or T. 
dbinom(x = 5,          # Use dbinom() to find P(k|n,p). Suppose k=5 success trials (say H is success)
       size = 10,      # total size is 10 
       prob = 0.5)     # prob is 0.5. Calculate n!/[k!(n-k)!] * p^k * (1-p)^(n-k). It is {0.2460938}

sample(c("H","T"), 1)  
sum(dbinom(x = 4:7, size = 10, prob = 0.5))  # (3) find sum when 4<-p<-7
pbinom(size = 10, prob = 0.5, q = 7) - pbinom(size = 10, prob = 0.5, q = 3)   # Method 2: use pbinom() to find P(k<-7)-P(k<-3)

sample(c("H","T"), 1)                 # (4) Plot probability distribution
k <- 0:10                            # set up a vector of possible outcomes where k is between 0 to 100
probability <- dbinom(x = k,          # Let x = k
                      size = 10,
                      prob = 0.5)
plot(x = k,                           
     y = probability,                 
     main = "Probability Distribution Function")

probability <- pbinom(q = k,          # Let q = k
                      size = 10,
                      prob = 0.5)
plot(x = k,                           
     y = probability,                 
     main = "Cumulative Probability Distribution")
####----------------------------------------------------------------------------------

#### Expected Value, Mean, and Variance #### 
mean(1:6)   # (1) find mean from 1 thru 6. It is {3.5}

set.seed(1)                     # use set.seed() to get a unique sample
sample(1:6,3, replace = TRUE)   # (2) sample from 1 thru 6 for 3 times. It is {1 4 1}

set.seed(1)
mean(sample(1:6, 1000, replace = TRUE)) # (3) Sample from 1 thru 6 for 1000 times. Find mean of the 1000 samples. It is {3.514}

var(1:7)    # find variance for numbers from 1 thru 7. It is {4.666667}
####----------------------------------------------------------------------------------

#### Continuous Random Variables #### 
f <- function(x) 3 / x^4     # (1) define function f, which is pdf fX(x)=3 / x^4, where x>1
g <- function(x) x*f(x)      # define function g, use g to find E(X)
h <- function(x) x^2 * f(x)  # define function h, use h to find E(X^2)

area <- integrate(f,                  # (2) find area under fX(x). Use function integrate() 
                  lower = 1,          # since x>1, lower bound is 1
                  upper = Inf)$value  # upper bound is infinity, use Inf
area                                  # It is {1}

EX <- integrate(g,                    # (3) find E(X).Integrate functin g
                lower = 1,
                upper = Inf)$value  
EX                                    # It is {1.5}

VarX <- integrate(h,                          # (4) find Var(X). Integrate function h
                  lower = 1,
                  upper = Inf)$value - EX^2   # note VarX = E(X^2)-EX^2
VarX                                          # It is {0.75}
####----------------------------------------------------------------------------------

#### Normal Distribution #### 
curve(dnorm(x),                # (1) use curve() and dnorm() to plot densities of normal distribution (PDF), where X~N(0,1)
      xlim = c(-3.5,3.5),      # limit x-axis between -3.5 to 3.5
      ylab = "Density",        
      main = "Standard Normal Distribution")

dnorm(x = c(-1.96,0,1.96))     # (2) find density at x=-1.96, x=0, x=1.96. It is {0.05844094 0.39894228 0.05844094}

curve(pnorm(x),                # (3) use pnorm() to plot cunumative densities of normal distribution (CDF)
      xlim = c(-3.5,3.5),     
      ylab = "Density",        
      main = "Standard Normal Cumulative Distribution")

pnorm(1.337)                       # (4)use pnorm() to find P(Z<-1.337). It is {0.9093887}

f <- function(x) {                 # Method 2: define function f, which is PDF for standard normal distribution  
  1/(sqrt(2*pi)) * exp(-0.5*x^2)   # where mean=0, SD=1, plug in PDF and get this equation
}
quants <- c(-1.96,0,1.96)          # define quants 
f(quants)                          # plut in -1.96,0,1.96 into f functin to check if f function is correct PDF. it is {0.05844094 0.39894228 0.05844094}
f(quants) == dnorm(quants)         # compare results from f function with dnorm fuction. It is {T T T}
integrate(f,                       # integrate f(x)dx to find P(Z<-1.337)
          lower = -Inf,            # lower bound: -infinity
          upper = 1.337)           # It is {0.9093887 with absolute error < 1.7e-07}


1-2*pnorm(-1.96)        # (5) find P(-1.96<-Z<-1.96). Use symmetry. It is {0.9500042}

pnorm(1.96) - pnorm(0)  # (6) find P(0<-Z<-1.96). It is {0.4750021}

pnorm(4,mean = 5, sd = 5) - pnorm(3, mean = 5, sd = 5)   # find P(3<-Y<-4) for Y~N(5,25). Note sd=5. It is {0.07616203}
####----------------------------------------------------------------------------------

#### Chi-squared Distribution #### 
curve(dchisq(x,df=3),     # (1) plot PDF for chi-squared  
      xlim = c(0,10),
      ylim = c(0,1),
      col = "blue",       # set col color to blue  
      ylab = "",          # set ylab="" to get rid of the default ylab dchisq(x,df=3)
      main = "pdf and cdf of Chi-Squared Distribution, M=3") # M here refers to df. 

curve(pchisq(x,df=3),     # (2) plot CDF for chi-squared  
      xlim = c(0,10),
      add = TRUE,         # add=TRUE to allow these two curves on a single graph
      col = "red")

legend("topleft",         # (3) add legend on topleft
       c("PDF","CDF"),
       col = c("blue","red"),
       lty = c(1,1))      # set line type as solid line by lty=c(1,1). If set lty=c(1,2), then CDF in the legend becomes dotted line. The larger the number, the smaller the dot. 

curve(dchisq(x,df = 1),   # (4) graph for df = 1 first
      xlim = c(0,15),
      xlab = "x",
      ylab = "Density",
      main = "Chi-Square Distributed Rndom Variables")

for (M in 2:7) {             # use for loop, where M starts from 2 to 7
  curve(dchisq(x, df = M),   # graph curves for df from 2 to 7
        xlim = c(0,15),
        add = TRUE,
        col = M)             # adjust colors for each iteration of the loop by col = M
}

legend("topright",        
       as.character(1:7),    # as.character()  
       col = 1:7,
       lty = 1,
       title = "df")
####----------------------------------------------------------------------------------

#### Student t Distribution #### 
curve(dnorm(x),            # (1) 1st, draw a standard normal distribution with dotted line
      xlim = c(-4,4),
      xlab = "x",
      lty = 2,
      ylab = "Density",
      main = "Densities of t Distributions")

curve(dt(x,df=2),          # use dt() draw student t-distribution 
      xlim = c(-4,4),
      col = 2,
      add = T)

curve(dt(x,df=4),
      xlim = c(-4,4),
      col = 3,
      add = T)

curve(dt(x,df=25),
      xlim = c(-4,4),
      col = 4,
      add = T)

legend("topright",
       c("N(0,1)","M=2","M=4","M=25"),
       col = 1:4,          # color is 1:4 since there're four lines 
       lty = c(2,1,1,1))   # line types is c(2,1,1,1) since want first line to dotted. 


curve(dnorm(x),            # (2) use for loop: draw student t-distribution for df from 2 to 10
      xlim = c(-4,4),
      xlab = "x",
      lty = 2,
      ylab = "Density",
      main = "Densities of t Distributions")

curve(dt(x,df=2),
      xlim = c(-4,4),
      col = 2,
      add = T)

for (M in 3:10) {             
  curve(dt(x, df = M),   
        xlim = c(-4,4),
        add = TRUE,
        col = M)             
}

legend("topright",
       c("N(0,1)","M=2 to 10"),
       col = 1:10,
       lty = c(2,1,1,1,1,1,1,1,1,1))
####----------------------------------------------------------------------------------

#### F Distribution #### 
pf(2,3,13,lower.tail = F)  # (1) prob. of F distribution, where P(Y>-2), df(numerator)=2, df(denominator)=13, lower.tail=F is for right tail of x (means P(Y>-2)), lower.tail=T is for left tail of x(means P(Y<-2))
                           # It is {0.1638271} 

x <- c(2, seq(2,10,0.01), 10)         # (2) Polygon (purple area): define x-axis
y <- c(0, df(seq(2,10,0.01),3,14),0)  # define y-axis
curve(df(x,3,14),                     # draw F distribution 
      ylim = c(0,0.8),
      xlim = c(0,10),
      ylab = "Density",
      main = "Density function")
polygon(x,y,col = "purple")
####----------------------------------------------------------------------------------

#### Random Sampling $ distribution of sample averages ####
sum(sample(1:6, 2, replace = T))    # (1) sum the random 2 sample draw. It is {7}. (There are 36 possibilities)

S <- 2:12    # Set of possible outcome is S={2,3,4,5,6,7,8,9,10,11,12}

PS <- c(1:6, 5:1)/36   # (2) Probabilites of S are: 1/36 for S=2,...6/36 for S=7,5/36 for S=8...1/36 for S=12
PS

ES <- sum(S * PS)      # (3) Expecatation of S is S*PS = 7
ES

VarS <- sum(           # (4) Variance of S is {5.833333}
   (S - c(ES))^2 * PS
  )
VarS

par(mfrow = c(1,2))           # (5) subdivide the plot panel into 1 by 2 array (1 row with 2 plots )
barplot(PS,                   # make barlpot() 
        ylim = c(0,0.2),
        xlab = "S",
        ylab = "Probability",
        col = "steelblue",
        space = 0,
        main = "Sum of two dice rolls")

probability <- rep(1/6,6)
names(probability) <- 1:6 
barplot(probability,
        ylim = c(0,0.2),
        xlab = "D",
        col = "steelblue",
        space = 0,
        main = "Outcome of a single dice roll") 
####----------------------------------------------------------------------------------

#### Monte Carlo Experiment (can be used for different distributions) #### 
n <- 100          # (1) choose sample size: n = 10
reps <- 10000    # (2) choose number of repititions = reps = 10000

samples <- replicate(reps,rnorm(n))   # (3) do random sampling
sample.avgs <- colMeans(samples)      # (4) find sample means for 100 samples of each column, repeat 10000 times, so we get total 10000 different sample means (each contains sample size 100)
is.vector(sample.avgs)                # check sample.avgs is a vector
head(sample.avgs)                     # print the first 6 sample averages. It is {0.1863254  0.4227131 -0.2537967 -0.3388349 -0.1578652  0.5950446}

hist(sample.avgs,          # (5) plot histogram  
     ylim = c(0,4),
     col = "steelblue",
     freq = F,             # freq = F, this normalized the area under histogram to 1
     breaks = 30,          # breaks = number of cubes  
     main = "Histogram of Sample Averages")
curve(dnorm(x,sd = 1/sqrt(n)),
      col = "red",
      lwd = ("2"),
      add = T)

reps <- 10000            # (6) for chi-squared: repetitions = 10000
DF <- 3                  # set DF = 3
Z <- replicate(reps,rnorm(DF))    
X <- colSums(Z^2)
hist(X,
     freq = F,
     col = "steelblue",
     breaks = 40,
     ylab = "Density",
     main = "")
curve(dchisq(x,df = DF),
      type = 'l',
      lwd = 2,
      col = "red",
      add = T) 
####----------------------------------------------------------------------------------

#### Large sample approx to sampling distribution #### 
set.seed(1)        # (1) congerging share f heads 
N <- 30000
Y <- sample(0:1, N, replace = T)
S <- cumsum(Y)
R <- S/(1:N)
plot(R,
     ylim = c(0.3,0.7),
     type = "l",
     col = "steelblue",
     lwd = 2,
     xlab = "n",
     ylab = "R_n",
     main = "Converging Share of heads in repeated coin tossing")
lines(c(0,N),
      c(0.5,0.5),
      col = "darkred",
      lty = 2,
      lwd = 1)

par(mfrow = c(2,2))    # (2) subdivide the plot panel into 2*2 array (2 plots in each row, total 2 rows)
reps <- 1000 
sample.sizes <- c(5, 20, 75, 100)

set.seed(123)

b <- c(5,10,15,30)     # vecotr of break numbers for histograms (n=5 with 5 bars, n=20 with 10 bars, n=75 with 15 bars, n=100 with 20 bars)

for (n in sample.sizes) {
  samplemean <- rep(0,reps)     # initialize vector of sample means
  stdsamplemean <- rep(0,reps)  # initialize vector of standardized sampple means
  
  for (i in 1:reps) {
    x <- rbinom(n, 1, 0.5)
    samplemean[i] <- mean(x)
    stdsamplemean[i] <- sqrt(n) * (mean(x) - 0.5) / 0.5
  }

hist(stdsamplemean,
     col = "gray12",
     freq = F,
     breaks = 40,
     xlim = c(-3,3),
     ylim = c(0,0.8),
     xlab = paste("n =", n),
     main = "")

curve(dnorm(x),
      lwd = 2,
      col = "deeppink",
      add = T)
}
####----------------------------------------------------------------------------------