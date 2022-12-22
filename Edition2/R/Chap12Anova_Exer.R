#Chapter 12 ANOVA

#Exercise 6 Simulation

nA <- 50         # set sample sizes
nB <- 50
nC <- 50
                 # create groups
Group <- rep(c("A","B","C"), c(nA, nB, nC))

counter <- 0
N <- 10^4

for (i in 1:N)
{
 a <- rnorm(nA, 20, 3)     # Draw samples
 b <- rnorm(nB, 20, 3)
 c <- rnorm(nC, 20, 3)
 X <- c(a, b, c)           # Combine into one vector

 Pvalue <- anova(lm(X ~ Group))$P[1] # Extract P-value
 if (Pvalue < 0.05)          # Reject H0?
  counter <- counter + 1     # If yes, increase counter

}

counter/N          # proportion of times H0 rejected
