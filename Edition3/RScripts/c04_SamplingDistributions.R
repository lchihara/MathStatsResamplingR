#Chapter 4
#Sampling Distributions

#Example 4.2
#Draw 1000 random samples of size 100 from the exponential
#distribution with lambda = 1/15
Xbar <- numeric(1000)    # space for results (vector of 0's)
for (i in 1:1000)
  {
    x <- rexp(100, rate = 1/15) # draw random sample of size 100
    Xbar[i] <- mean(x)          # compute mean, save in position i
   }

df <- data.frame(Xbar)
ggplot(df, aes(Xbar)) + geom_histogram(bins = 10)
ggplot(df, aes(sample = Xbar)) + geom_qq() + geom_qq_line()
mean(Xbar)
sd(Xbar)

#Example 4.3
#Sampling distribution of max from Unif[0,1]

maxY <- numeric(1000)
for (i in 1:1000)
{
  y <- runif(12) # draw random sample of size 12
  maxY[i] <- max(y) # find max, save in position i
}
df <- data.frame(maxY)
ggplot(df, aes(maxY)) + geom_histogram(bins = 10)

#----------------------------------------
#Example 4.6
#Sum of two values drawn from two different Poisson distributions
X <- rpois(10^4, 5)  # Draw 10^4 values from Pois(5)
Y <- rpois(10^4, 12) # Draw 10^4 values from Pois(12)
W <- X + Y

df1 <- data.frame(W)
df2 <- data.frame(x = 2:35, y = dpois(2:35,17))
ggplot(df1, aes(W)) +
  geom_histogram(aes(y=stat(density)), color = "white",
                 breaks=seq(2, 36, by = 2)) +
  geom_line(data = df2, aes(x = x, y = y)) +
  geom_point(data = df2, aes(x = x, y = y), pch = 1) + xlab("")

mean(W)      #compare to theoretical, lambda = 17
var(W)

#Example 4.7
#Sampling distribution of mean of sample of size 30 from Gamma(5, 2)
Xbar <- numeric(1000)
for (i in 1:1000)
  {
    x <- rgamma(30, shape = 5, rate = 2)
    Xbar[i] <- mean(x)
    }

df <- data.frame(Xbar)
ggplot(df, aes(x = Xbar)) +
  geom_histogram(aes(y = stat(density)), color = "white", bins = 10) +
  stat_function(fun = dnorm, args = list(mean = 5/2, s = 0.204)) +
  labs(x = "Means", y = "Density")
ggplot(df, aes(sample = Xbar)) + geom_qq() + geom_qq_line()
mean(Xbar)
sd(Xbar)

#----------------------------------------------
#Example 4.10
#
dbinom(25, 120, .3)
pbinom(25, 120, .3)


