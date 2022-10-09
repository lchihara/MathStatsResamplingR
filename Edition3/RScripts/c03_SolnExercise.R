#Chapter 3: Permutation tests
library(resampledata3)
library(ggplot2)
library(dplyr)

# Exercise 7 Checking different test statistics that will give same P-value
N <- 10^4 - 1
table(FlightDelays$Carrier)
tapply(FlightDelays$Delay, FlightDelays$Carrier, mean)
tapply(FlightDelays$Delay, FlightDelays$Carrier, sum)

observedSumUA <- 17949
observedmeanUA <- 15.98308
observedmeanDiff <- 15.98308 - 10.09738

sumUA <- numeric(N)
meanUA <- numeric(N)
meanDiff <- numeric(N)
set.seed(2)
for (i in 1:N) {
  index <- sample(4029, 1123, replace = FALSE)
  sumUA[i] <- sum(Delay[index])
  meanUA[i] <- mean(Delay[index])
  meanDiff[i] <- mean(Delay[index]) - mean(Delay[-index])
}

2 * (sum(sumUA >= observedSumUA) + 1) / (N + 1)  #P-value

2 * (sum(meanUA >= observedmeanUA) + 1) / (N + 1)  #P-value

2 * (sum(meanDiff >= observedmeanDiff) + 1) / (N + 1)  #P-value
