##Chapter 6: Estimation
library(resampledata3)
library(ggplot2)

#Section 6.1.2
#example 6.7
x <- c(1, 2, 2, 3)
g <- function(theta) sum(log(1 + (x-theta)ˆ2))
optimize(g, interval = c(0, 4))

logL <- function(theta) sum(log(dcauchy(x, theta)))
optimize(logL, interval = c(0, 4), maximum = TRUE)


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


##To plot histogram with density overlap

df <- data.frame(wind)
df2 <- data.frame(x = c(2,14))

#----------------
#
ggplot(df, aes(x = wind))  +
  geom_histogram(aes(y = stat(density)), bins = 10, color = "white") +
  stat_function(fun = dweibull, args = list(shape = 3.169, scale = 7.661)) +
  scale_x_continuous(breaks = seq(2,14, by = 2))+
  labs(x = "(m/s)", y = "Density")

#End wind speed case study

#-----------------------------------------
# Example 6.13
N <- 10^4
Xbar2 <- numeric(N)
Max <- numeric(N)
for (i in 1:N)
{
  x <- runif(25, 0, 12) # draw 25 from Unif[0, 12]
  Xbar2[i] <- 2 * mean(x)
  Max[i] <- 26/25 * max(x)
}
mean(Xbar2)
sd(Xbar2)
mean(Max)
sd(Max)
#Here we scale the axes to be the same. You may need to adjust this setting for
#your simulation.
df <- data.frame(Xbar2, Max)
library(gridExtra)
p1 <- ggplot(df, aes(Xbar2)) +
  geom_histogram(binwidth = .5, center = .25, color = "white") +
  labs(x = "Means") + xlim(c(8,16))
p2 <- ggplot(df, aes(Max)) +
  geom_histogram(binwidth = .5, center = .25, color = "white") +
  labs(x = "Maximums") + xlim(c(8,16))
grid.arrange(p1, p2, nrow = 2)

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

#Example 6.15
funMSE1 <- function(x, n)x*(1-x)/n
funMSE2 <- function(x, n)n*(1-x)*x/(n+2)ˆ2 + (1-2*x)ˆ2/(n+2)ˆ2
ggplot(data.frame(x = c(0,1)), aes(x = x)) +
  stat_function(fun = funMSE1, args = list(n = 16), lty = 1) +
  stat_function(fun = funMSE2, args = list(n = 16), lty = 2) +
  labs(x = "p", y = "MSE")