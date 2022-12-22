##Chapter 4: The Bootstrap
#R Scripts
#

#----------------------------------------------------
#Example 5.2

my.sample <- rgamma(16, 1, 1/2)

N <- 10^5
my.boot <- numeric(N)
for (i in 1:N)
 {
  x <- sample(my.sample, 16, replace = TRUE)  #draw resample
  my.boot[i] <- mean(x)                     #compute mean, store in my.boot
  }

 hist(my.boot)  #bootstrap distribution of mean
 mean(my.boot)  #mean
 sd(my.boot)    #bootstrap SE

#-----------------------------------------------------
#Example 5.3
##Arsenic in wells in Bangladesh
Bangladesh <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Bangladesh.csv")

Arsenic <- Bangladesh$Arsenic
#Alternatively,
#Arsenic <- subset(Bangladesh, select = Arsenic, drop = TRUE)

hist(Arsenic)

qqnorm(Arsenic)
qqline(Arsenic)

n <- length(Arsenic)
N <- 10^4

arsenic.mean<-numeric(N)

for (i in 1:N)
{
   x <- sample(Arsenic, n, replace = TRUE)
   arsenic.mean[i] <- mean(x)
}

hist(arsenic.mean, main = "Bootstrap distribution of means")
abline(v = mean(arsenic.mean), col = "blue", lty = 2)


qqnorm(arsenic.mean)
qqline(arsenic.mean)

mean(arsenic.mean)                 #bootstrap mean
mean(arsenic.mean) - mean(Arsenic) #bias
sd(arsenic.mean)                   #bootstrap SE

sum(arsenic.mean > 161.3224)/N
sum(arsenic.mean < 89.75262)/N

##------------------------------------------------------------------
#Example 5.4 Skateboard

Skateboard <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Skateboard.csv")


testF <- subset(Skateboard, select = Testosterone, subset = Experimenter == "Female",
            drop = TRUE)
testM <- subset(Skateboard, select = Testosterone, subset = Experimenter == "Male",
            drop = TRUE)
observed <- mean(testF) - mean(testM)

nf <- length(testF)
nm <- length(testM)

N <- 10^4

TestMean <- numeric(N)

for (i in 1:N)
{
  sampleF <- sample(testF, nf, replace = TRUE)
  sampleM <- sample(testM, nm, replace = TRUE)
  TestMean[i] <- mean(sampleF) - mean(sampleM)
}

hist(TestMean, main = "Bootstrap distribution of difference in means",
     xlab="Means")
abline(v = observed , col = "blue", lty = 2)

qqnorm(TestMean)
qqline(TestMean)

mean(testF) - mean(testM)
mean(TestMean)
sd(TestMean)

quantile(TestMean,c(0.025,0.975))

mean(TestMean)- observed  #bias

#-------
#Permutation test for Skateboard means

testAll <- subset(Skateboard, select = Testosterone, drop = TRUE)
#testAll <- Skateboard$Testosterone

N <- 10^4 - 1  #set number of times to repeat this process

#set.seed(99)
result <- numeric(N) # space to save the random differences
for(i in 1:N)
  {
  index <- sample(71, size = nf, replace = FALSE) #sample of numbers from 1:71
  result[i] <- mean(testAll[index]) - mean(testAll[-index])
}
(sum(result >= observed)+1)/(N + 1)  #P-value


hist(result, xlab = "xbar1 - xbar2",
main="Permutation distribution for testosterone levels")
#lines(density(result))
abline(v = observed, col = "blue")

qqnorm(result)
qqline(result)

##-------------------------------------------------------------
#Sectin 5.4.1 Matched pairs for Diving data
Diving2017 <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Diving2017.csv")
Diff <- Diving2017$Final - Diving2017$Semifinal
n <- length(Diff)

N <- 10^5
result <- numeric(N)

for (i in 1:N)
{
  dive.sample <- sample(Diff, n, replace = TRUE)
  result[i] <- mean(dive.sample)
}

hist(result)
quantile(result, c(0.025, 0.975))

#--------------------------------------------

##Example 5.5 Verizon

#Verizon <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Verizon.csv")
Time.ILEC <- subset(Verizon, select = Time, Group == "ILEC", drop = TRUE)
Time.CLEC <- subset(Verizon, select = Time, Group == "CLEC", drop = TRUE)

observed <- mean(Time.ILEC) - mean(Time.CLEC)

n.ILEC <- length(Time.ILEC)
n.CLEC <- length(Time.CLEC)

time.ILEC.boot <- numeric(N)
time.CLEC.boot <- numeric(N)

time.diff.mean <- numeric(N)

N <- 10^4
#set.seed(100)
for (i in 1:N)
 {
  ILEC.sample <- sample(time.ILEC, n.ILEC, replace = TRUE)
  CLEC.sample <- sample(time.CLEC, n.CLEC, replace = TRUE)
  time.ILEC.boot[i] <- mean(ILEC.sample)
  time.CLEC.boot[i] <- mean(CLEC.sample)
  time.diff.mean[i] <- mean(ILEC.sample) - mean(CLEC.sample)
}

#bootstrap for ILEC
hist(time.ILEC.boot, main = "Bootstrap distribution of ILEC means",
    xlab = "means")
abline(v = mean(time.ILEC), col = "blue")

qqnorm(Time.ILEC.boot)
qqline(Time.ILEC.boot)

#bootstrap for CLEC
hist(Time.CLEC.boot, main = "Bootstrap distribution of CLEC means",
    xlab = "means")
abline(v = mean(Time.CLEC), col = "blue")

qqnorm(Time.CLEC.boot)
qqline(Time.CLEC.boot)

#Different in means
hist(time.diff.mean, main = "Bootstrap distribution of difference in means")
abline(v = mean(time.diff.mean), col = "red")
abline(v = observed1, col = "blue")

qqnorm(time.diff.mean)
qqline(time.diff.mean)

mean(time.diff.mean)
quantile(time.diff.mean, c(0.025, 0.975))


##-----------------------------------------------
##Section 5.5 Verizon trimmed means

#
Verizon <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Verizon.csv")
Time.ILEC <- subset(Verizon, select = Time, Group == "ILEC", drop = TRUE)
Time.CLEC <- subset(Verizon, select = Time, Group =="CLEC", drop = TRUE)

n.ILEC <- length(Time.ILEC)
n.CLEC <- length(Time.CLEC)

N <- 10^4
time.diff.trim <- numeric(N)

#set.seed(100)
for (i in 1:N)
{
  x.ILEC <- sample(Time.ILEC, n.ILEC, replace = TRUE)
  x.CLEC <- sample(Time.CLEC, n.CLEC, replace = TRUE)
  time.diff.trim[i] <- mean(x.ILEC, trim = .25) - mean(x.CLEC, trim = .25)
}

hist(time.diff.trim, main = "Bootstrap distribution of difference in trimmed means",
    xlab = "difference in trimmed means")
abline(v = mean(time.diff.trim),col = "red")
abline(v = mean(Time.ILEC,trim = .25) - mean(Time.CLEC, trim = .25),
     col = "blue")


qqnorm(time.diff.trim)
qqline(time.diff.trim)

mean(time.diff.trim)
quantile(time.diff.trim, c(0.025,0.975))
##-----------------------------------------------------------------
#Section 5.5 Other statistics
# Verizon ratio of means

#Time.ILEC and Time.CLEC created above
#n.ILEC, n.CLEC created above

N <- 10^4
time.ratio.mean <- numeric(N)

set.seed(100)
for (i in 1:N)
{
  ILEC.sample <- sample(Time.ILEC, n.ILEC, replace = TRUE)
  CLEC.sample <- sample(Time.CLEC, n.CLEC, replace = TRUE)
  time.ratio.mean[i] <- mean(ILEC.sample)/mean(CLEC.sample)
}


hist(time.ratio.mean, main = "Bootstrap distribution of ratio of means",
     xlab = "ratio of means")
abline(v = mean(time.ratio.mean), col = "red")
abline(v = mean(Time.ILEC)/mean(Time.CLEC), col = "blue")

qqnorm(time.ratio.mean)
qqline(time.ratio.mean)

mean(time.ratio.mean)
sd(time.ratio.mean)
quantile(time.ratio.mean, c(0.025, 0.975))

###--------------------------------------------------------------
#Example 5.7 Relative risk example

highbp <- rep(c(1,0),c(55,3283))   #high blood pressure
lowbp <- rep(c(1,0),c(21,2655))    #low blood pressure

N <- 10^4
boot.rr <- numeric(N)
high.prop <- numeric(N)
low.prop <- numeric(N)

for (i in 1:N)
{
   x.high <- sample(highbp,3338, replace = TRUE)
   x.low  <- sample(lowbp, 2676, replace = TRUE)
   high.prop[i] <- sum(x.high)/3338
   low.prop[i]  <- sum(x.low)/2676
   boot.rr[i] <- high.prop[i]/low.prop[i]
}

ci <- quantile(boot.rr, c(0.025, 0.975))

hist(boot.rr, main = "Bootstrap distribution of relative risk",
    xlab = "relative risk")
abline(v = mean(boot.rr), col = "red")
abline(v = 2.12, col = "blue")
legend("topleft", legend = c("Observed RR","Mean of bootstrap dist."),
 col=c("blue","red"), lty = 1)

plot(range(low.prop), range(high.prop),
    xlab = "proportion in low blood pressure group",
    ylab = "proportion in high blood pressure group",
    type = "n", xlim = c(0, .02), ylim = c(0, .03))

temp <- ifelse(high.prop < 1.31775*low.prop, 1, 0)

points(low.prop[temp == 1], high.prop[temp == 1], pch = 3, col = "green")

temp2 <- ifelse(high.prop > 3.687*low.prop, 1, 0)
points(low.prop[temp2 == 1], high.prop[temp2 == 1], pch = 3, col = "green")

temp3 <- temp + temp2
 points(low.prop[temp3 == 0], high.prop[temp3 == 0], pch = 16, col = "blue")

abline(v = mean(low.prop), col = "red", lwd = 2)
abline(h = mean(high.prop),col = "red", lwd = 2)
abline(0,2.12, lty = 2, lwd = 2)
abline(0,ci[1],col = "blue", lwd = 2)
abline(0,ci[2], col = "blue", lwd = 2)
legend("topleft", legend = c("Sample relative risk", "bootstrap CI"), lty = c(2, 1),
      lwd = c(2, 2), col = c("black", "blue"))
title("Scatter plot of bootstrapped proportions")
