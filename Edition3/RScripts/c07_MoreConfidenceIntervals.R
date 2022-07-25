#Chapter 7 Classical confidence intervals
library(resampledata3)
library(dplyr)
library(ggplot2)

#Example 7.1
#Confidence intervals of mean of samples of size 30 drawn from N(25, 4)
counter <- 0                     # set counter to 0
df <- data.frame(x = c(22, 28), y = c(1,100))
p <- ggplot(df, aes(x = x, y = y)) + geom_vline(xintercept = 25)

for (i in 1:1000)
  {
    x <- rnorm(30, 25, 4)          # draw a random sample of size 30
    L <- mean(x) - 1.96*4/sqrt(30) # lower limit
    U <- mean(x) + 1.96*4/sqrt(30) # upper limit
    if (L < 25 && 25 < U)          # check if 25 is in interval
      counter <- counter + 1       # if yes, increase counter by 1
    if (i <= 100)                  # plot first 100 intervals
      p <- p + annotate("segment", x = L, xend = U, y = i, yend = i)
   }

p
counter/1000           # proportion of times interval contains mu.

#------------------------------------
#Section 7.1.2
#Confidence intervals for mean of samples drawn from normal
#distribution, mean and variance unknown
N <- 10^4
w <- numeric(N)
n <- 15             # sample size
for (i in 1:N)
  {
    x <- rnorm(n, 25, 7)  # draw 15 from N(25, 7^2)
    xbar <- mean(x)
    s <- sd(x)
    w[i] <- (xbar-25) / (s/sqrt(n))
   }

df <- data.frame(w)
ggplot(df, aes(sample = w)) + geom_qq(size = .8) +
  geom_qq_line()

#-----------------------------------
#Example 7.5
pt(2.8, 27)
qt(0.95, 27)

#------------------------------------
#Example 7.6
girls <- NCBirths2004 %>% filter(Gender == "Female") %>%
  pull(Weight)
t.test(girls, conf.level = .99)$conf

#----------------------------------------------
#Example 7.7
#Samples from right-skewed Gamma(5,2)
tooLow <- 0                  # set counter to 0
tooHigh <- 0                 # set counter to 0
n <- 20  # sample size
q <- qt(0.975, n-1)          # quantile
N <- 10^5
for (i in 1:N)
  {
    x <- rgamma(n, shape = 5, rate = 2)
    xbar <- mean(x)
    s <- sd(x)
    L <- xbar - q*s/sqrt(n)
    U <- xbar + q*s/sqrt(n)
    if (U < 5/2)               # Does right endpt miss 5/2?
      tooLow <- tooLow + 1     # If yes, increase counter
    if (5/2 < L)               # Does left endpt miss 5/2?
      tooHigh <- tooHigh + 1   # If yes, increase counter
   }
tooLow/N
tooHigh/N

#-------------------------------------------
#Example 7.8
t.test(Response ~ Treatment, data = Reading)$conf

#------------------------------------------
#Example 7.14
t.test(NCBirths2004$Weight, alt = "greater")$conf

#-----------------------------------------
#Example 7.17
prop.test(1385, 2193, conf.level = .9)$conf

prop.test(1385, 2193, conf.level = .9, alt = "greater")$conf

#----------------------------------------
#Example 7.20

prop.test(c(172, 223), c(674, 676))$conf

#---------------------------------------
#Example 7.21
#Bootstrap t confidence interval
Arsenic <- Bangladesh$Arsenic
xbar <- mean(Arsenic)
N <- 10^4
n <- length(Arsenic)
Tstar <- numeric(N)
for (i in 1:N)
  {
    x <- sample(Arsenic, size = n, replace = T)
    Tstar[i] <- (mean(x)-xbar) / (sd(x)/sqrt(n))
    }
quantile(Tstar, c(0.025, 0.975))

xbar - quantile(Tstar, c(0.975, 0.025)) * sd(Arsenic)/sqrt(n)

#--------------------------------------------
#Example 7.22
#Bootstrap t CI for difference in means
TimeILEC <- Verizon \%>\% filter(Group == "ILEC") \%>\% pull(Time)
TimeCLEC <- Verizon \%>\% filter(Group == "CLEC") \%>\% pull(Time)

thetahat <- mean(TimeILEC) - mean(TimeCLEC)
nx <- length(TimeILEC)  # nx=1664
ny <- length(TimeCLEC)  # ny=23
SE <- sqrt(var(TimeILEC)/nx + var(TimeCLEC)/ny)

N <- 10^4
Tstar <- numeric(N)
for (i in 1:N)
  {
    bootx <- sample(TimeILEC, nx, replace = TRUE)
    booty <- sample(TimeCLEC, ny, replace = TRUE)
    Tstar[i] <- (mean(bootx) - mean(booty) - thetahat) /
      sqrt(var(bootx)/nx + var(booty)/ny)
}
thetahat - quantile(Tstar, c(.975, .025)) * SE
t.test(TimeILEC, TimeCLEC)$conf # for comparison

#---------------------------------------------
#Example 7.3
#Bootstrap t with estimated standard errors iterated bootstrap
Arsenic <- Bangladesh$Arsenic
estimate <- mean(Arsenic, trim = 0.25)  # 35.95985

N <- 10^4 # outer loop
N2 <- 10^2 # inner loop
n <- length(Arsenic)
Tstar <- numeric(N)
estimateStar <- numeric(N)
seStar <- numeric(N)

for (i in 1:N)
  {
    x <- sample(Arsenic, size = n, replace = T)
    
    # Inner loop to estimate standard error based on x
    estimate2 <- numeric(N2)
    for (j in 1:N2)
      {
        x2 <- sample(x, size = n, replace = T)
        estimate2[j] <- mean(x2, trim = 0.25)
       }
    
    estimateStar[i] <- mean(x, trim = 0.25)
    seStar[i] <- sd(estimate2)
    Tstar[i] <- (estimateStar[i] - estimate) / seStar[i]
    }


sd(estimateStar) # Standard error 
quantile(Tstar, c(0.025, 0.975))
# Bootstrap t interval
estimate - quantile(Tstar, c(.975, .025)) * sd(estimateStar)

#Ordinary t interval with bootstrap SE
estimate + qt(c(0.025, 0.975), n-1) * sd(estimateStar)
