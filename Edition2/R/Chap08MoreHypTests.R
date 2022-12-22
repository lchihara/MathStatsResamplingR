#Chap08MoreHypTest

#Section 8.2
Bangladesh <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Bangladesh.csv")

t.test(Bangladesh$Arsenic, mu = 100, alt = "greater")

Arsenic <- Bangladesh$Arsenic
N <- 10^5

observedT <- t.test(Arsenic, mu = 100)$statistic
xbar <- mean(Arsenic)
n <- length(Arsenic)
Tstar <- numeric(N)
for (i in 1:N)
{
 bootx <- sample(Arsenic, n , replace = TRUE)
 Tstar[i] <- (mean(bootx) - xbar)/(sd(bootx)/sqrt(n))
}

hist(Tstar)
abline(v = observedT)

(sum(Tstar >= observedT) + 1)/(N + 1)
