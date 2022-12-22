##Chapter 5 The Boostrap

#Exercises

#------------------------------------------------
#10 (medians)
##
ne <- 10000 # n even
no <- 10001 # n odd

wwe <- rnorm(ne) # draw random sample of size ne
wwo <- rnorm(no) # draw random sample of size no

N <- 10^4
even.boot <- numeric(N) #save space
odd.boot <- numeric(N)
set.seed(10)
for (i in 1:N)
 {
  x.even <- sample(wwe, ne, replace = TRUE)
  x.odd <- sample(wwo, no, replace = TRUE)
  even.boot[i] <- median(x.even)
  odd.boot[i]  <- median(x.odd)
 }

par(mfrow = c(2, 1))
hist(even.boot, xlim = c(-1, 1))  #set x range to be
hist(odd.boot, xlim = c(-1, 1))  #same in both plots
par(mfrow = c(1, 1))               #reset to original

#-----------------------------------
#Exercise 20
Diving2017 <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Diving2017.csv")
N <- 10^5
result <- numeric(N)
for (i in 1:N)
{
  index <- sample(12, replace = TRUE)
  Dive.boot <- Diving2017[index, ]
  result[i] <- mean(Dive.boot$Final) - median(Dive.boot$Semifinal)
}

hist(result)
quantile(result, c(0.025, 0.975))
