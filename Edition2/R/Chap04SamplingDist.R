###Chapter 4: Sampling Distributions

#---------------------------------------------
#Example 4.2: Sampling distribution from Exp(1/15)
Xbar <- numeric(1000)
#set.seed(300)
for (i in 1:1000)
{
  x <- rexp(100, rate = 1/15)
  Xbar[i] <- mean(x)
}

hist(Xbar, main="Simulated sampling distribution", xlab="means")

qqnorm(Xbar)
qqline(Xbar)

mean(Xbar)
sd(Xbar)

#----------------------------------------------------
##Example 4.3: Sampling Dist from Unif[0,1]

maxY <- numeric(1000)
#set.seed(100)
for (i in 1:1000)
 {
   y <- runif(12)        #draw random sample of size 12
   maxY[i] <- max(y)     #find max, save in position i
 }

hist(maxY, main = "", xlab = "maximums")

#To create a histogram with a density curve imposed
#scale bars to have area one with prob=TRUE option
hist(maxY, main = "", xlab = "maximums", prob = TRUE)

#add pdf to histogram
curve(12*x^{11}, col = "blue", add = TRUE)

#---------------------------------------------
#Example 4.6 Sum of Poisson random variables

X <- rpois(10^4, 5)   #Draw 10^4 values from Pois(5)
Y <- rpois(10^4, 12)   #Draw 10^4 values from Pois(12)
W <- X + Y

hist(W, prob = TRUE)  #prob = TRUE, scales hist to 1
lines(2:35, dpois(2:35, 17), type = "b") #overlay pmf for Pois(17)

mean(W)
var(W)

#------------------------------------------------
#Example 4.7
#Sampling distribution simulation
#Sample of size 30 from gamma r=5, lambda=2

#set.seed(10)
Xbar <- numeric(1000)
for (i in 1:1000)
  {
    x <- rgamma(30, shape = 5, rate = 2)
    Xbar[i] <- mean(x)
  }

hist(Xbar, main = "Distribution of means")

qqnorm(Xbar)
qqline(Xbar)

mean(Xbar)
sd(Xbar)
sum(Xbar > 3)/1000
#alternatively
mean(Xbar > 3)

#----------------------------------------------
#Example 4.11 R Note
dbinom(25, 120, .3)

pbinom(25, 120, .3)
