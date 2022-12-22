##Chapter 6: Estimation

#----------------------------------------------------------
##Wind speed case study: fitting the Weibull distribution

#------------------------------------------------------------
# This function takes input shape parameter k and
# the data to compute
# (1/k)+ (1/n)*sum (log(xi)) +(1/alpha)sum xi^klog(xi)=0
# where alpha= sum xi^k.

weibull.shape <- function(k, data)
{
  numer <- colSums(outer(data, k, "^") * log(data))
  denom <- colSums(outer(data, k, "^"))
  numer/denom - 1/k - mean(log(data))
}

#-----
# This function takes input shape parameter k
# and data to compute
#  k^{th} root of (1/n) sum xi^k
# n=number of data values

weibull.scale <- function(k, data)
{
  mean(data^k)^(1/k)
}

##-----
# uniroot is a built-in R function which estimates the root
# of a function.
# Provide function, any arguments needed for function,
# and a guess of values two values around root.
# Function values must be opposite signs at lower
# and upper guess.

#Now, we do the data specific commands
wind <- Turbine$AveSpeed
#alternatively, wind <- subset(Turbine, select=AveSpeed, drop=TRUE)

#estimate the shape parameter k
uniroot(weibull.shape, data = wind, lower = 1,upper = 5)

# With estimate of shape parameter, now find estimate
# of scale parameters lambda

weibull.scale(3.169, wind)

# Plot histogram with density curve overlap
# The prob=TRUE option scales histogram to area 1.

hist(wind, main = "Distribution of average wind speeds",
    xlab = "meters/sec", prob = TRUE)
curve(dweibull(x, 3.169, 7.661), add = TRUE, col = "blue", lwd = 2)

dev.new()
plot.ecdf(wind,main = "ECDF of wind data")
curve(pweibull(x,3.169,7.661), add = TRUE, col = "blue", lwd = 2)

# Now for the chi-square goodness of fit test
# Get the deciles
q <- qweibull(seq(.1, .9, by = .1), 3.169, 7.661)

#range of wind
range(wind)

#encompass range of wind
q <- c(0, q, 14)

# Get the counts in each sub-interval. The plot=F command
# suppresses plot and gives statistics
hist(wind, breaks = q, plot = F)$counts

# repeat above but store output
count <- hist(wind,breaks = q, plot = F)$counts
expected <- length(wind)*.1

# compute chi-square test statistic
sum((count-expected)^2/expected)

#End wind speed case study

#-----------------------------------------
# Example 6.13
# Simulation comparing two estimators for uniform
Xbar <- numeric(1000)
maxY <- numeric(1000)

#set.seed(100)
for (i in 1:1000)
 {
   x <- runif(25,0,12) #sample n=25 from Unif[0,1]
   Xbar[i] <- 2*mean(x)
   maxY[i] <- 26/25*max(x)
 }

 #mean and standard deviation of the method of moments estimate
 mean(Xbar)
 sd(Xbar)

 #mean and sd of estimate from MLE
 mean(maxY)
 sd(maxY)

 par(mfrow = c(1,2))
 hist(Xbar, xlim = c(8,16), ylim = c(0,650), xlab = "means",
    main = "2*Sample mean")
 hist(maxY,  xlim = c(8, 16), ylim = c(0, 650), xlab = "maximums",
    main = "25/24*maximum")
 par(mfrow = c(1,1))


#----------------------
#Example 6.14
#Relative efficiency for the Wind Speed Case Study
# Theta = P(wind > 5), nonparametric and parametric estimates
n <- length(wind)
theta1 <- mean(wind > 5)
theta2 <- pweibull(5, 3.169, 7.661, lower.tail = FALSE)
theta1 # 0.75
theta2 # 0.7720827
eta1 <- quantile(wind, .1)
eta2 <- qweibull(.1, 3.169, 7.661)
eta1 # 3.77
eta2 # 3.766038

# nonparametric bootstrap to find standard errors
set.seed(23)
N <- 10^4
boot.theta1 <- numeric(N)
boot.shape <-  numeric(N)
boot.scale <-  numeric(N)
boot.theta2 <- numeric(N)
boot.eta1 <- numeric(N)
boot.eta2 <- numeric(N)
for (i in 1:N)
{
  boot.wind <- wind[sample(1:n, replace=TRUE)]
  boot.theta1[i] <- mean(boot.wind > 5)
  boot.shape[i] <- uniroot(weibull.shape, data=boot.wind, lower=1,upper=5)$root
  boot.scale[i] <- weibull.scale(boot.shape[i], boot.wind)
  boot.theta2[i] <- pweibull(5, boot.shape[i], boot.scale[i], lower.t = FALSE)
  boot.eta1[i] <- quantile(boot.wind, .1)
  boot.eta2[i] <- qweibull(.1, boot.shape[i], boot.scale[i])
}

qqnorm(boot.theta1) # discrete, normal
qqnorm(boot.theta2) # continuous, normal
qqnorm(boot.eta1) # discrete, irregular, roughly normal
qqnorm(boot.eta2) # continuous, very normal

sd(boot.theta1) # 0.03331767
sd(boot.theta2) # 0.02342771
var(boot.theta1) / var(boot.theta2) # 2.022504

# Formula standard error, for comparison
sqrt(theta1 * (1-theta1) / n) # standard error = 0.03340766

sd(boot.eta1) # 0.2161054
sd(boot.eta2) # 0.1839509
var(boot.eta1) / var(boot.eta2) # 1.380154

# parametric bootstrap to find standard errors
set.seed(23)
pboot.theta1 <- numeric(N)
pboot.shape <-  numeric(N)
pboot.scale <-  numeric(N)
pboot.theta2 <- numeric(N)
pboot.eta1 <- numeric(N)
pboot.eta2 <- numeric(N)
for (i in 1:N)
{
  boot.wind <- rweibull(n, 3.169, 7.661)
  pboot.theta1[i] <- mean(boot.wind > 5)
  pboot.shape[i] <- uniroot(weibull.shape, data=boot.wind, lower=1,upper=5)$root
  pboot.scale[i] <- weibull.scale(pboot.shape[i], boot.wind)
  pboot.theta2[i] <- pweibull(5, pboot.shape[i], pboot.scale[i], lower.t=FALSE)
  pboot.eta1[i] <- quantile(boot.wind, .1)
  pboot.eta2[i] <- qweibull(.1, pboot.shape[i], pboot.scale[i])
}
etaRange <- range(boot.eta1, boot.eta2, pboot.eta1, pboot.eta2)

qqnorm(pboot.theta1) # discrete, slight neg skewness
qqnorm(pboot.theta2) # continuous, normal
qqnorm(pboot.eta1) # very normal, continuous
qqnorm(pboot.eta2) # very normal, continuous

sd(pboot.theta1) # 0.03198990
sd(pboot.theta2) # 0.02604409
var(pboot.theta1) / var(pboot.theta2) # 1.508716
# Similar to the nonparametric bootstrap, though relative efficiency differs

sd(pboot.eta1) # 0.2823103
sd(pboot.eta2) # 0.2100147
var(pboot.eta1) / var(pboot.eta2) # 1.806982


##-----------------------------------------------------
