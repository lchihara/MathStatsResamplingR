#Chapter 9 Regression
#

#Section 9.2
Spruce <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Spruce.csv")

plot(Spruce$Di.change, Spruce$Ht.change)
cor(Spruce$Di.change, Spruce$Ht.change)

plot(Ht.change ~ Di.change, data = Spruce)

#Example 9.3
spruce.lm <- lm(Di.change ~ Ht.change, data = Spruce)
spruce.lm

plot(Spruce$Ht.change, resid(spruce.lm), ylab = "residuals")
abline(h = 0)
lines(smooth.spline(Spruce$Ht.change, resid(spruce.lm), df = 3), col = "blue")

#Example 9.8
Skating2010 <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Skating2010.csv")
skate.lm <- lm(Free ~ Short, data = Skating2010)
summary(skate.lm)

#Section 9.5
N <- 10^4
cor.boot <- numeric(N)
beta.boot <- numeric(N)
alpha.boot <- numeric(N)
yPred.boot <- numeric(N)
n <- 24       #number of skaters
for (i in 1:N)
 {
  index <- sample(n, replace = TRUE)    #sample f rom 1, 2, ... n
  Skate.boot <- Skating2010[index, ]

  cor.boot[i] <- cor(Skate.boot$Short, Skate.boot$Free)

  #recalculate linear model estimates
  skateBoot.lm <- lm(Free ~ Short, data = Skate.boot)
  alpha.boot[i] <- coef(skateBoot.lm)[1]   # new intercept
  beta.boot[i] <- coef(skateBoot.lm)[2]    # new slope
  yPred.boot[i] <- alpha.boot[i] + 60 * beta.boot[i]  #recompute Y^
  }

  mean(cor.boot)
  sd(cor.boot)
  quantile(cor.boot, c(0.025, 0.975))

  hist(cor.boot, main = "Bootstrap distribuiton of correlation",
      xlab = "Correlation")
  observed <- cor(Skating2010$Short, Skating2010$Free)
  abline(v = observed, col = "blue")    #add line at observed cor.

#-------------------------------------------------------
# Section 9.5.1 Permutation test

 N <- 10^5 - 1
 n <- nrow(Skating2010)   #number of observations
 result <- numeric(N)
 observed <- cor(Skating2010$Short, Skating2010$Free)
 for (i in 1:N)
 {
   index <- sample(n , replace = FALSE)
   Short.permuted <- Skating2010$Short[index]
   result[i] <- cor(Short.permuted, Skating2010$Free)
 }

 (sum(observed <= result) + 1)/(N+1)    #P-value

#----------------------------------------------
#Chapter 9.6.1 Inference for logistic regression
Fatalities <-read.csv("http://sites.google.com/site/chiharahesterberg/data2/Fatalities.csv")

 fit <- glm(Alcohol ~ Age, data = Fatalities, family = binomial)
 data.class(fit)  # is a "glm" object, so for help use:
 help(glm)

 fit          # prints the coefficients and other basic info
 coef(fit)    # the coefficients as a vector
 summary(fit) # gives standard errors for coefficients, etc.

 x <- seq(17, 91, length = 500) # vector spanning the age range
 # compute predicted probabilities
 y1 <- exp(-.123 - .029*x) / (1 + exp(-.123 - .029*x))
 y2 <- plogis(coef(fit)[1] + coef(fit)[2] * x)

 plot(Fatalities$Age, Fatalities$Alcohol,
      ylab = "Probability of alcohol")
 lines(x, y2)

 # Full bootstrap - slope coefficient, and prediction at age 20
 N <- 10^3
 n <- nrow(Fatalities)                   # number of observations
 alpha.boot <- numeric(N)
 beta.boot <- numeric(N)
 pPred.boot <- numeric(N)

 for (i in 1:N)
 {
   index <- sample(n, replace = TRUE)
   Fatal.boot <- Fatalities[index, ]     # resampled data

   fit.boot <- glm(Alcohol ~ Age, data = Fatal.boot,
                   family = binomial)
   alpha.boot[i] <- coef(fit.boot)[1]    # new intercept
   beta.boot[i] <- coef(fit.boot)[2]     # new slope
   pPred.boot[i] <- plogis(alpha.boot[i] + 20 * beta.boot[i])
 }

 quantile(beta.boot, c(.025, .975))      # 95% percentile intervals
 quantile(pPred.boot, c(.025, .975))

 par(mfrow=c(2,2))                       # set layout
 hist(beta.boot, xlab = "beta", main = "")
 qqnorm(beta.boot, main = "")

 hist(pPred.boot, xlab = "p^", main = "")
 qqnorm(pPred.boot, main = "")

#--------------------
 help(predict.glmm)                # for more help on predict

 n <- nrow(Fatalities)             # number of observations
 x <- seq(17, 91, length = 500)      # vector spanning the age range
 df.Age <- data.frame(Age = x)     # data frame to hold
     # explanatory variables, will use this for making predictions

 plot(Fatalities$Age, Fatalities$Alcohol,
      ylab = "Probability of alcohol")
 for (i in 1:25)
 {
   index <- sample(n, replace = TRUE)
   Fatal.boot <- Fatalities[index, ]     # resampled data

   fit.boot <- glm(Alcohol ~ Age, data = Fatal.boot,
                   family = binomial)
   pPred <- predict(fit.boot, newdata = df.Age, type = "response")
   lines(x, pPred)
 }

#end fatalities
#---------------------
