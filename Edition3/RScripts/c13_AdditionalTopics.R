#Chapter 13 Additional Topics
library(resampledata3)
library(ggplot2)
library(dplyr)

#Example 13.2
#Control variates
n <- 10^4
U <- runif(n, 0, .5)
Y <- sin(U)
mean(Y)
mean(Y - U) + 0.25
var(Y)
var(Y - U)
betahat <- lm(Y ~ U)$coef[2]
betahat
var(Y - betahat * U)

#--------------------------------------------
#Example 13.4 Monte carlo integration
N <- 10^5
x <- runif(N, 1, 3)   # draw from Unif[1, 3]
out <- 2*exp(-x^2)    # evaluate h at each random x
mean(out)      
sd(out) / sqrt(N)  

integrate(function(x) exp(-x^2), 1, 3) #adaptive

#-------------------------------------------------
#Example 13.5
#first function computes prior, except for the constant
f0 <- function(u) exp(-25 * abs(u-0.5))
f1 <- function(u) f0(u) * u^285 * (1-u)^635 # prior*like
f2 <- function(u) u * f1(u)             # for expected value
x <- runif(10^6)
a <- mean(f0(x))       # constant for prior
K <- mean(f1(x))       # constant in denom. of E[theta|x=215]
b <- mean(f2(x))       # numerator of E[theta|x=215]
c(a, K, b, b/K)        # output all

# Plot the prior and posterior
df <- data.frame(x = x, y = f0(x)/a, w = f1(x)/K)
ggplot(df) +
  geom_line(aes(x = x, y = y, color = "Prior")) +
  geom_line(aes(x = x, y = w, color = "Post"), lty = 2) +
  scale_color_manual(name = NULL,
                     values = c("Prior" = "black", "Post" = "red"))

#------------------------------------------------
#Example 13.6
x <- rnorm(10^6, 45.53, 1.953)
out <- 0.088*x^(3.069)
mean(out)

#---------------------------------------------
#Example 13.8
#European stock option
K <- 700     # strike price
mu <- 500    # mean of the stock price at option date
sigma <- 120 # sd

# P(option has value)
pnorm(K, mu, sigma, lower.tail = FALSE)

# define function g(x) = h(x)*f(x)
g <- function(x) (x - K) * dnorm(x, mu, sigma)
# expected payout
integrate(g, K, Inf)
# expected payout given there is a payout
integrate(g, K, Inf)$value /
  pnorm(K, mu, sigma, lower.tail = FALSE)

#The option lower.tail = FALSE} to pnorm gives
#1 - pnorm(K, mu, sigma).

#We next estimate the integral using Monte Carlo integration without
#importance sampling. The pmax function computes the coordinate-wise
#maximum of two vectors.

# Simulation, no ImpSamp
N <- 10^5
X <- rnorm(N, mu, sigma)
payout <- pmax(X-K, 0)
mean(payout)
sd(payout) / sqrt(N)
mean(payout[payout > 0])
mean(payout > 100)

#Now integrate using importance sampling
X2 <- rnorm(N, K, sigma)               # drawing from g
w2 <- dnorm(X2, mu, sigma) / dnorm(X2, K, sigma)
# w2(x) = f(x)/g(x)
Y2 <- pmax(0, X2-K) * w2               # h(x) * w(x)
mean(Y2)
sd(Y2) / sqrt(N)
sd(Y2) / sd(payout)
var(payout) / var(Y2)                  # relative efficiency

lambda <- 1 / sigma        # same st deviation as the normal
X3 <- K + rexp(N, lambda)  # g ~ shifted exponential
w3 <- dnorm(X3, mu, sigma) / dexp(X3 - K, lambda)
Y3 <- pmax(0, X3-K) *w3
mean(Y3)
sd(Y3) / sqrt(N)
sd(Y3) / sd(payout)
var(payout) / var(Y3)

df <- data.frame(x = X3[1:300], y = Y3[1:300])
ggplot(df, aes(x = x, y = y)) + geom_point(size = .5) +
  labs(x = "X", y = "Y = h(x)f(x)/g(x)")

#------------------------------------------------------
#Example 13.9
#Reproduce figure
likelihood <- function(theta) theta^110 * (1-theta)^90
logLikelihood <- function(theta) 110 * log(theta) +
  90 * log(1-theta)

N <- 180
theta <- sort(runif(N))  # Sampling from  uniform dist.

ggplot(data.frame(x = c(0,1)), aes(x = x)) +
  stat_function(fun = likelihood)

w <- likelihood(theta)
weight <- w / sum(w)
cumsumwt <- cumsum(weight)
df <- data.frame(x = theta, y = cumsumwt, w = weight)

ggplot(df, aes(x = x, y = y)) + geom_step() + geom_point()

#------------------------------------
#Example 13.11
#EM algorithm
lambda1 <- 1     #initial value for lambda1
for (i in 1:20)
{
  beta <- 12/(lambda1 + 3)
  lambda1 <- (lambda1 + 5)/(beta + 1)
  lambda2 <- 10/(beta + 1)
}
beta
lambda1
lambda2