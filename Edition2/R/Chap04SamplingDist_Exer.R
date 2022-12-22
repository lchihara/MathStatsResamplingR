##Chapter Sampling Distributions
##Exercises
##R Scripts
##
##-----------------------------
#Exercise 4
pop <- c(3, 5, 6, 6, 8, 11, 13, 15, 19, 20)
N <- 10^4
Xbar <- numeric(N)

for (i in 1:N)
{
 samp <- sample(pop, 4, replace = TRUE)
 Xbar[i] <- mean(samp)
}

hist(Xbar)
mean(Xbar < 11)

#----------------------------------------------
#Exercise 4.6
Recidivism <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Recidivism.csv")
 N <- 10^4
 phat <- numeric(N)
 n <- 25

 for (i in 1:N)
 {
  samp <- sample(Recidivism$Recid, n)
  phat[i] <- mean(samp == "Yes")
 }

#c)  change n <- 250

#----------------------------------------------------------------------------
#Exercise 19
## X1,X2,..X10 ~ N(20, 8^2), Y1, Y2,..Y15 ~ N(16,7^2)
## W = mean(X)+mean(Y)
 W <- numeric(1000)
 set.seed(0)
    for (i in 1:1000)
    {
       x <- rnorm(10, 20, 8)  #draw 10 from N(20, 8^2)
       y <- rnorm(15, 16, 7)  #draw 15 from N(16, 7^2)
       W[i] <- mean(x) + mean(y) #save sum of means
    }

    hist(W)

    mean(W < 40)


#--------------------
#Exercise 22

X <- runif(1000, 40, 60)
Y <- runif(1000, 45, 80)

total <- X + Y

hist(total)

#----------------
#33 Finite pop simulation

N <- 400 # population size
n <- 5 # sample size

finpop <- rexp(N, 1/10) # Create a finite pop. of size N=400 from
# Exp(1/10)
hist(finpop) # distribution of your finite pop.
mean(finpop) # mean (mu) of your pop.
sd(finpop) # stdev (sigma) of your pop.
sd(finpop)/sqrt(n) # theoretical standard error of sampling
# dist. of mean(x), with replacement
sd(finpop)/sqrt(n) * sqrt((N-n)/(N-1)) # without replacement

Xbar <- numeric(1000)
for (i in 1:1000)
{
x <- sample(finpop, n) # Random sample of size n (w/o replacement)
Xbar[i] <- mean(x) # Find mean of sample, store in my.means
}
hist(Xbar)

qqnorm(Xbar)
qqline(Xbar)

mean(Xbar)
sd(Xbar) # estimated standard error of sampling
             # distribution

#----------------------------
#34
W <- numeric(1000)
for (i in 1:1000)
{
x <- rnorm(20, 25, 7)
W[i] <- var(x)
}
mean(W)
var(W)
hist(W)

qqnorm(W)
qqline(W)
