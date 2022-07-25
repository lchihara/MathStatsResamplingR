#Chapter 12 ANOVA
library(resampledata3)
library(ggplot2)
library(dplyr)

#Example 12.1
anova(lm(Weight ~ MothersAge, data = ILBoys))
anova(lm(Weight ~MothersAge, data = ILBoys))$F[1] #Extract F stat

summary(aov(Weight ~MothersAge, data = ILBoys)) #same

#Section 12.1.2 Permutation Test Approach
#Checking the normality condition
ggplot(ILBoys, aes(sample = Weight)) + geom_qq() +
  geom_qq_line() + facet_wrap(. ~ MothersAge)

#Permutation test
observed <- anova(lm(Weight ~ MothersAge, data = ILBoys))$F[1]
n <- length(ILBoys$Weight)
N <- 10^4 - 1
results <- numeric(N)
for (i in 1:N)
  {
    index <- sample(n)
    Wt.perm <- ILBoys$Weight[index]
    results[i] <- anova(lm(Wt.perm ~ MothersAge, data = ILBoys))$F[1]
  }

(sum(results >= observed) + 1) / (N + 1)   # P value
