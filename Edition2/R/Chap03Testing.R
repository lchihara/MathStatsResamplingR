#Chapter 3 Introduction to Hypothesis Testing: Permutation Tests
##-------------------------------------
##Section 3.3
#Beerwings <- read.csv("https://sites.google.com/site/chiharahesterberg/data2/Beerwings.csv")

tapply(Beerwings$Hotwings, Beerwings$Gender, mean)

observed <- 14.5333- 9.3333 #store observed mean differences

#Get hotwings variable
hotwings <- Beerwings$Hotwings

#Alternative way:
hotwings <- subset(Beerwings, select = Hotwings, drop = TRUE)
#drop = TRUE to convert hotwings to a vector (without this, hotwings will be a
#30x1 data frame

#set.seed(0)
N<- 10^5-1  #set number of times to repeat this process
 result <- numeric(N) # space to save the random differences
 for(i in 1:N)
  {
  index <- sample(30, size=15, replace = FALSE) # sample of numbers from 1:30
  result[i] <- mean(hotwings[index]) - mean(hotwings[-index])
}

##Plot

hist(result, xlab = "xbarM - xbarF", main = "Permutation distribution for hot wings")
abline(v = observed, col = "blue", lty=5)

#-------------------------
#Another visualization of distribution
plot.ecdf(result)
abline(v = observed, col = "blue", lty = 5)


#Compute P-value
(sum(result >= observed)+1)/(N+ 1)  #P-value


#----------------------------------------
#Example 3.4 Verizon
#Permutation test

Verizon <- read.csv("https://sites.google.com/site/chiharahesterberg/data2/Verizon.csv")

tapply(Verizon$Time, Verizon$Group, mean)


Time <- subset(Verizon, select = Time, drop = T)
Time.ILEC <- subset(Verizon, select = Time, Group == "ILEC", drop = TRUE)
Time.CLEC <- subset(Verizon, select = Time, Group == "CLEC", drop = TRUE)

observed <- mean(Time.ILEC) - mean(Time.CLEC)
observed


N <- 10^4-1  #set number of times to repeat this process
#set.seed(99)
result <- numeric(N) # space to save the random differences
for(i in 1:N) {
  index <- sample(1687, size = 1664, replace = FALSE) #sample of numbers from 1:1687
  result[i] <- mean(Time[index]) - mean(Time[-index])
}

hist(result, xlab = "xbar1 - xbar2",
      main = "Permutation Distribution for Verizon repair times")
abline(v = observed, col = "blue", lty = 5)

(sum(result <= observed) + 1)/(N + 1)  #P-value


#-------------------------------------------------------
#Example 3.6, Verizon cont.
#median, trimmed means

tapply(Verizon$Time, Verizon$Group, median)

#Difference in means
observed <- median(Time.ILEC) - median(Time.CLEC)
observed

#Differnce in trimmed means
observed2 <- mean(Time.ILEC, trim = .25) - mean(Time.CLEC, trim = .25)
observed2

N <- 10^4-1  #set number of times to repeat this process
#set.seed(99)
result  <- numeric(N) # space to save the random differences
result2 <- numeric(N)
for(i in 1:N) {
  index <- sample(1687, size=1664, replace = FALSE) #sample of numbers from 1:1687
  result[i] <- median(Time[index]) - median(Time[-index])
  result2[i] <- mean(Time[index], trim = .25) - mean(Time[-index], trim = .25)
}

hist(result, xlab = "median1 - median2",
    main = "Permutation Distribution for medians")
abline(v = observed, col = "blue", lty = 5)

#P-value difference in means
(sum(result <= observed) + 1)/(N+ 1)


hist(result2, xlab = "trimMean1 - trimMean2",
     main = "Permutation Distribution for trimmed means")
abline(v = observed, col = "blue", lty = 5)

#P-value difference in trimmed means
(sum(result2 <= observed2) + 1)/(N+ 1)

#------------------------------------------------
#Example 3.6, Verzion continued
#
#difference in proportion of time > 10
#and ratio of variances
observed3 <- mean(Time.ILEC > 10) - mean(Time.CLEC > 10)
observed3

#ratio of variances
observed4 <- var(Time.ILEC)/var(Time.CLEC)
observed4

N <- 10^4-1  #set number of times to repeat this process
#set.seed(99)
 result3 <- numeric(N)
 result4 <- numeric(N)

 for(i in 1:N) {
  index <- sample(1687, size = 1664, replace = FALSE)
  result3[i] <- mean(Time[index] > 10) - mean(Time[-index] > 10)
  result4[i] <- var(Time[index])/var(Time[-index])
  }



hist(result3, xlab = "Difference in proportions",  main = "Repair times > 10 hours")
abline(v = observed3, lty = 5, col = "blue")
#P-value difference in proportion
 (sum(result3 <= observed3) + 1)/(N+ 1)  #P-value


hist(result4, xlab = "variance1/variance2",  main = "Ratio of variances")
abline(v = observed4, lty = 5, col = "blue")


#P-value ratio of variances
 (sum(result4 <= observed4) + 1)/(N+ 1)  #P-value

#--------------------------------------
#Example 3.8
Recidivism <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Recidivism.csv")

k <- complete.cases(Recidivism$Age25)
Recid2 <- ifelse(Recidivism$Recid[k] == "Yes", 1, 0)
Age25.2 <- Recidivism$Age25[k]

table(Age25.2)
tapply(Recid2, Age25.2, mean)
observed <- .365 - .306

N <- 10^4 - 1
result <- numeric(N)

for (i in 1:N)
{
  index <- sample(17019, size = 3077, replace = FALSE)
  result[i] <- mean(Recid2[index]) - mean(Recid2[-index])
}

2* (sum(result >= observed) + 1)/(N + 1)

#---------------------
#Section 3.4 Matched Pairs
Diving2017 <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Diving2017.csv")

Diff <- Diving2017$Final - Diving2017$Semifinal
observed <- mean(Diff)

N <- 10^5 - 1
result <- numeric(N)

for (i in 1:N)
{
  Sign <- sample(c(-1, 1), 12, replace = TRUE)
  Diff2 <- Sign*Diff
  result[i] <- mean(Diff2)
}

hist(result)
abline(v = observed, col = "blue")

2* (sum(result >= observed) + 1)/(N + 1)
