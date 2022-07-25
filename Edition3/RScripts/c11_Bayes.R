#Chapter 11
#Bayesian methods
library(resampledata3)
library(ggplot2)
library(dplyr)

#Example 11.1
theta <- seq(0, 1, by = .1)
prior <- c(0, .02, .03, .05, .1, .15, .2, .25, .15, .05, 0)
likelihood <- theta * (1 - theta)^2
constant <- sum(prior * likelihood)
posterior <- prior * likelihood / constant
posterior
sum(theta * prior)            # prior mean 
sum(theta * posterior)        # posterior mean

#continued
likelihood2 <- theta^3 * (1 - theta)^5  # 3 success, 5 fail
constant2 <- sum(prior * likelihood2)
posterior2 <- prior * likelihood2 / constant2
posterior2

likelihood3 <- theta^2 * (1 - theta)^3
constant3 <- sum(posterior * likelihood3)
posterior3 <- posterior * likelihood3 / constant3
posterior3                  # not shown, same as posterior2
sum(theta*posterior2)       # posterior mean

ggplot(df, aes(x = theta, y = prior)) +
  geom_point() + geom_line(lty = 1) +
  geom_point(aes(y = posterior)) +
  geom_line(aes(y = posterior),  lty = 2) +
  geom_point(aes(y = posterior2)) +
  geom_line(aes(y = posterior2), lty = 3)

#----------------------------------------------------
#Example 11.3
qbeta(.025, 111, 91)
qbeta(.975, 111, 91)
1-pbeta(.5, 111, 91)

ggplot(data.frame(x = c(0,1)), aes(x = x)) +
  stat_function(fun = dbeta, aes(lty = "2"),
                args = list(shape1 = 1, shape2 = 1)) +
  stat_function(fun = dbeta, aes(lty = "1"),
                args = list(shape1 = 111, shape2 = 91)) +
  scale_linetype_manual(values = c("2" = 2, "1" = 1),
                        labels = c("Posterior", "Prior"),
                        guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  labs(x = "", y = "Density") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.key = element_blank())

#-------------------------------------------
#Section 11.5 Sequential data

n <- c(1874, 1867, 1871, 1868, 1875, 1875)
X <- c(52, 41, 55, 49, 39, 39)
alpha <- X     # vector of posterior parameters
beta <- n - X  # vector of posterior parameters
N <- 10^5                    # replications
theta <- matrix(0.0, nrow = N, ncol = 6)
for (j in 1:6)
  {
    theta[, j] <- rbeta(N, alpha[j], beta[j])
    }
probBest <- numeric(6)       # vector for results
best <- apply(theta, 1, max) # maximum of each row
for (j in 1:6)
  {
    probBest[j] <- mean(theta[, j] == best)
   }

#probBest contains probabilities of each of the six arms
#being best

df <- data.frame(theta[1:10^4,])
names(df)
ggplot(df, aes(x = X1, y = X3)) + geom_point(size = .5) +
  geom_abline(slope = 1, intercept = 0) +
  annotate("text", x = 0.037, y = 0.042, parse = TRUE,
           label = "theta[3] > theta[1]") +
  annotate("text", x = 0.042, y = 0.037, parse = TRUE,
           label = "theta[1] > theta[3]") +
  labs(x = expression(theta[1]), y=expression(theta[3]))

#----------------------------------------
probBest
#