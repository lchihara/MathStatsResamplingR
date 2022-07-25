##Chapter 5 Bootstrap
library(resampledata3)
library(ggplot2)
library(dplyr)

#Draw random sample of size 16 from Gamma(1, 1/2)
#Example 5.2
N <- 10^5
mean.boot <- numeric(N)
for (i in 1:N)
{
  x <- sample(gamSample, 16, replace = TRUE) # draw resample
  mean.boot[i] <- mean(x) # compute mean, store in mean.boot
}

mean(mean.boot)
sd(mean.boot)

df <- data.frame(mean.boot)
ggplot(df, aes(mean.boot)) +
  geom_histogram(bins = 20, color = "white")

#-----------------
#Example 5.3
ggplot(Bangladesh, aes(Arsenic))  +
  geom_histogram(bins = 10, color = "white")
  ggplot(Bangladesh, aes(sample = Arsenic))  +
  geom_qq() + geom_qq_line()

Arsenic <- Bangladesh$Arsenic

n <- length(Arsenic)
N <- 10^4
mean.boot <- numeric(N)
for (i in 1:N)
  {
    x <- sample(Arsenic, n, replace = TRUE)
    mean.boot[i] <- mean(x)
   }

df <- data.frame(mean.boot)
ggplot(df, aes(mean.boot)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = mean(mean.boot), color = "red", lty = 2)
ggplot(df, aes(sample = mean.boot)) + geom_qq() + geom_qq_line()

mean(mean.boot)  
mean(mean.boot)-mean(Arsenic)
sd(mean.boot)

quantile(mean.boot, c(0.025, 0.975))

#----------------------------------
#Example 5.4 Skateboarders
testF <- Skateboard %>% filter(Experimenter == "Female") %>%
  pull(Testosterone)
testM <- Skateboard %>% filter(Experimenter == "Male") %>%
  pull(Testosterone)

observed <- mean(testF) - mean(testM)     #observed difference
observed

nf <- length(testF)  #sample size
nm <- length(testM)  #sample size

N <- 10^4
mean.boot <- numeric(N)

for (i in 1:N)
{
  resampleF <- sample(testF, nf, replace = TRUE)
  resampleM <- sample(testM, nm, replace = TRUE)
  mean.boot[i] <- mean(resampleF)-mean(resampleM)
}

df <- data.frame(mean.boot)
ggplot(df, aes(mean.boot)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = observed, color = "green", lty = 2)
ggplot(df, aes(sample = mean.boot)) + geom_qq() + geom_qq_line()

mean(testF) - mean(testM)
mean(mean.boot)
sd(mean.boot)
quantile(mean.boot, c(0.025, 0.975))
mean(mean.boot) - (mean(testF) - mean(testM))  # bias

#-------------
#Example 5.6
#Verizon data

TimeILEC <- Verizon %>% filter(Group=="ILEC") %>% pull(Time)
TimeCLEC <- Verizon %>% filter(Group=="CLEC") %>% pull(Time)

observed <- mean(TimeILEC)/mean(TimeCLEC)
observed

nILEC <- length(TimeILEC)
nCLEC <- length(TimeCLEC)

N <- 10^4
ratio.boot <- numeric(N)

for (i in 1:N)
{
  resampleILEC <- sample(TimeILEC, nILEC, replace = TRUE)
  resampleCLEC <- sample(TimeCLEC, nCLEC, replace = TRUE)
  ratio.boot[i] <- mean(resampleILEC)/mean(resampleCLEC)
}

df <- data.frame(ratio.boot)
ggplot(df, aes(ratio.boot)) +
  geom_histogram(bins = 15, color="white") +
  xlab("Ratio of means") +
  geom_vline(xintercept = observed, lty = 2, color = "red") +
  geom_vline(xintercept = mean(ratio.boot), lty = 3, color = "blue")

ggplot(df, aes(sample = ratio.boot)) +
  geom_qq() + geom_qq_line()

mean(ratio.boot)
sd(ratio.boot)
quantile(ratio.boot, c(0.025, 0.975))
mean(ratio.boot) - mean(TimeILEC)/mean(TimeCLEC)

#Example 5.7 Verizon continued
#modifications to above for proportion of times the ILEC
#delay time was greater than 24 hours
N <- 10^4

prop.boot <- numeric(N)
for (i in 1:N)
  {
    resampleILEC <- sample(TimeILEC, nILEC, replace = TRUE)
    prop.boot[i] <- mean(resampleILEC >  24)
  }

quantile(prop.boot, c(0.025, 0.975))
#--------------------
#Example 5.8
#Faith among Black Americans
genZ <- rep(c(1, 0), c(118, 139))
genX <- rep(c(1, 0), c(965, 1510))

observed <- mean(genZ) - mean(genX) # observed diff.
observed

N <- 10^4
prop.boot <- numeric(N)
for (i in 1:N)
  {
    resampleZ <- sample(genZ, 257, replace = TRUE)
    resampleX <- sample(genX, 2475, replace = TRUE)
    prop.boot[i] <- mean(resampleZ) - mean(resampleX)
}

quantile(prop.boot, c(0.025, 0.975))

#----------------------------------------
#Example 5.6
#Relative risk

highbp <- rep(c(1,0), c(55,3283))  #high bp sample
lowbp <- rep(c(1,0), c(21,2655))   #low  bp sample

N <- 10^4
rr.boot <- numeric(N)

for (i in 1:N)
 {
    resampleHigh <- sample(highbp, 3338, replace = TRUE)
    resampleLow <- sample(lowbp, 2676, replace = TRUE)
    
    rr.boot[i] <- mean(resampleHigh)/mean(resampleLow)  #rel.
    #risk
  }

quantile(rr.boot, c(0.025, 0.975))