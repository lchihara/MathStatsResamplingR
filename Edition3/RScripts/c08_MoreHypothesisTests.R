#Chapter 8 More Hypothesis Tests
library(resampledata3)
library(ggplot2)
library(dplyr)

#Example 8.4
t.test(Bangladesh$Arsenic, mu = 100, alt = "Greater")

#Bootstrap t test approach
Arsenic <- Bangladesh$Arsenic
observedT <- t.test(Arsenic, mu = 100)$statistic
xbar <- mean(Arsenic)
n <- length(Arsenic)
N <- 10^5
Tstar <- numeric(N)

for (i in 1:N)
  {
    bootx <- sample(Arsenic, n, replace = TRUE)
    Tstar[i] <- (mean(bootx)- xbar)/(sd(bootx)/sqrt(n))
  }

(sum(Tstar >= observedT)+1)/(N+1)


#------------------------------------------
#Example 8.5
#Comparing two means
t.test(Weight ~ Smoker, data = NCBirths2004, alt = "greater")

#-------------------------------------------
#Example 8.6
prop.test(c(108, 51), c(143, 119))

#-------------------------------------------
#Example 8.15
sum(dbinom(5:8, 8, 0.3185)) 
1 - pbinom(4, 8, 0.3185)   #same

#---------------------------------------------
#Example 8.19
binom.test(7, 21, 0.5)

pbinom(7, 21, 0.5696755)
1 - pbinom(6, 21, 0.1458769)

#-----------------------------