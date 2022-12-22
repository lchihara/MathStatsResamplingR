#Chapter 12 ANOVA
ILBoys <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/ILBoys.csv")
anova(lm(Weight ~ MothersAge, data = ILBoys))

anova(lm(Weight ~ MothersAge, data = ILBoys))$F[1]

summary(aov(Weight ~ MothersAge, data = ILBoys))

#-------------------------------- 
#Section 12.1.2 Permutation test approach
observed <- anova(lm(Weight ~ MothersAge, data = ILBoys))$F[1]

n <- length(ILBoys$Weight)
N <- 10^4 - 1
results <- numeric(N)
for (i in 1:N)
{
 index <- sample(n)
 Weight.perm <- ILBoys$Weight[index]
 results[i] <- anova(lm(Weight.perm ~ MothersAge, data = ILBoys))$F[1]
}

(sum(results >= observed) + 1) / (N + 1)
