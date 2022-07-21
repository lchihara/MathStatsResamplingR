#Chapter 3 Permutation Tests
library(resampledata3)
library(dplyr)
library(ggplot2)

#Section 3.3

#Beerwings data set
Beerwings %>% group_by(Gender) %>% summarize(mean(Hotwings))
observed <- 14.5333 - 9.3333  # store observed mean difference
observed

hotwings <- Beerwings$Hotwings
# Alternative syntax using the dplyr package:
# hotwings <- Beerwings %>% pull(Hotwings)

N <- 10^5 - 1        # number of times to repeat this process
result <- numeric(N) # space to save the random differences
for (i in 1:N)
  { # sample of size 15, from 1 to 30, without replacement
    index <- sample(30, size = 15, replace = FALSE)
    result[i] <- mean(hotwings[index]) - mean(hotwings[-index])
}

ggplot() + geom_histogram(aes(result), bins = 8) +
  geom_vline(xintercept = observed, linetype="dashed")

(sum(result >= observed) + 1)/(N + 1) # P-value

#-----
#Verizon data set

Verizon %>% group_by(Group) %>% summarize(mean(Time))
Time <- Verizon$Time
TimeILEC <- Verizon %>% filter(Group == "ILEC") %>% pull(Time)
TimeCLEC <- Verizon %>% filter(Group == "CLEC") %>% pull(Time)

observed <-  mean(TimeILEC) - mean(TimeCLEC)
observed

N <- 10^4-1
result <- numeric(N)
for (i in 1:N)
  {
    index <- sample(1687, size = 1664, replace = FALSE)
    result[i] <- mean(Time[index]) - mean(Time[-index])
}

ggplot() + geom_histogram(aes(result), bins = 8) +
  geom_vline(xintercept = observed, linetype = "dashed")

(sum(result <= observed) + 1)/(N + 1)

#---------
#Other statistics
#Example 3.6
#median
observed <- median(TimeILEC) - median(TimeCLEC)
N <- 10^4-1
result <- numeric(N)
for (i in 1:N)
{
  index <- sample(1687, size = 1664, replace = FALSE)
  result[i] <- median(Time[index]) - median(Time[-index])
}
(sum(result <= observed) + 1)/(N + 1)  # P-value

#trimmed mean
#modifications to above
observed  <- (mean(TimeILEC, trim = .25) -
                mean(TimeCLEC, trim = .25))
#within for loop above, change to:
result[i] <- (mean(Time[index], trim = .25) -
                mean(Time[-index], trim = .25))


#for proportion of time ILEC times > 10
observed <- mean(TimeILEC > 10) - mean(TimeCLEC > 10)
#and in the for loop, modify to
result[i] <- mean(Time[index] > 10) - mean(Time[-index] > 10)

#for ratio of variances
observed  <- var(TimeILEC) / var(TimeCLEC)
result[i] <- var(Time[index]) / var(Time[-index])

#Recidivism case study
#Example 3.8

library(tidyr)
data <- Recidivism %>% drop_na(Age25) %>%
  select(Age25, Recid)
table(data$Age25)
proportions(table(data$Age25, data$Recid), 1)

Recid <- data$Recid   # create vector
observed <- .365 - .306
N <- 10^4 - 1
result <- numeric(N)
for (i in 1:N)
  {
    index <- sample(17019, size = 3077, replace = FALSE)
    result[i] <- mean(Recid[index]=="Yes") -
      mean(Recid[-index]=="Yes")
  }
2*(sum(result >= observed)+1)/(N+1)

#Example 3.9
#Pew Research study on Faith among Black Americans
pooled.data <- rep(c(1,0), c(1068, 1283))  # create vector
observed <- (963/2094) - (105/257)  # observed difference
# (Mill-Gen Z)
N <- 10^4-1
result <- numeric(N)

for (i in 1:N)
  {
    index <- sample(2351, 2094, replace = FALSE)
    result[i] <- mean(pooled.data[index]) -
      mean(pooled.data[-index])
    }
2 * (sum(result >= observed)+1) / (N+1)

#-----------------------------------------

#Section 3.4 Matched pairs
#Diving
Diff <- Diving2017$Final - Diving2017$Semifinal   #difference in two scores
observed <- mean(Diff)                            #mean of difference

N <- 10^5-1
result <- numeric(N)

for (i in 1:N)
  {
    Sign <- sample(c(-1,1), 12, replace=TRUE)   #random vector of 1's or -1's
    Diff2 <-  Sign*Diff                         #random pairs (a-b) -> (b-a)
    result[i] <- mean(Diff2)                    #mean of difference
   }

ggplot() + geom_histogram(aes(result), bins = 8) +
  geom_vline(xintercept = mean(observed), linetype="dashed")

2 * (sum(result >= observed)+1) / (N+1)       #P-value
