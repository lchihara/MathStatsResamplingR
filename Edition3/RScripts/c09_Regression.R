#Chapter 9
#Regression
library(resampledata3)
library(ggplot2)
library(dplyr)

#Section 9.2
#base R
cor(Spruce$Ht.change, Spruce$Dichange)
#dplyr package
Spruce %>% summarize(coor = cor(Ht.change, Di.change))

#ggplot2 package
qqplot(Spruce, aes(x = Ht.change, y = Di.change)) + geom_point()
#base R
plot(Di.change ~ Ht.change, data = Spruce)

#-------------------------------------------------
#Example 9.3

spruce.lm <- lm(Di.change ~ Ht.change, data = Spruce)
spruce.lm

ggplot(Spruce, aes(x = Ht.change, y = Di.change)) + geom_point() +
   geom_smooth(method = lm, se = FALSE)

fitted(spruce.lm)
predict(spruce.lm) #same

(nrow(Spruce) -1) * var(Spruce$Ht.change)

#-----------------------------------------------
#Section 9.3
Spruce$Residuals <- resid(spruce.lm)
ggplot(Spruce, aes(x = Ht.change, y = Residuals)) +
  geom_point() + geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se = FALSE, span = 2)

#----------------------------------------------
#Example 9.8
skate.lm <- lm(Free ~ Short, data = Skating2010)
summary(skate.lm)

#Section 9.5
#Bootstrapping correlation, slope, intercept, 

N <- 10^4
cor.boot <- numeric(N)
beta.boot <- numeric(N)
alpha.boot <- numeric(N)
yPred.boot <- numeric(N)
n <- nrow(Skating2010)              # number of skaters = 24
for (i in 1:N)
{
  index <- sample(n, replace = TRUE) # sample from 1,2,...,n
  Skate.boot <- Skating2010[index, ] # resampled data
  
  cor.boot[i] <- cor(Skate.boot$Short, Skate.boot$Free)
  
  #recalculate linear model estimates
  skateBoot.lm <- lm(Free ~ Short, data = Skate.boot)
  alpha.boot[i] <- coef(skateBoot.lm)[1] # new intercept
  beta.boot[i] <- coef(skateBoot.lm)[2]  # new slope
  yPred.boot[i] <- alpha.boot[i] + 60 * beta.boot[i]
}

mean(cor.boot)
sd(cor.boot)
quantile(cor.boot, c(.025,.975))

observed <- cor(Skating2010$Short, Skating2010$Free)

df <- data.frame(cor.boot, beta.boot, alpha.boot, yPred.boot)

ggplot(df, aes(x = cor.boot)) +
  geom_histogram(bins = 20, color = "white") +
  geom_vline(xintercept =  observed, color = "red", lty = 2)

#--------------------------------------------
#Section 9.5.1 Permutation Tests

N <- 9999
n <- nrow(Skating2010)  # number of observations
result <- numeric(N)
observed <- cor(Skating2010$Short, Skating2010$Free)
for (i in 1:N)
{
  index <- sample(n, replace=FALSE)
  Short.permuted <- Skating2010$Short[index]
  result[i] <- cor(Short.permuted, Skating2010$Free)
}
(sum(observed <= result) + 1) / (N + 1)  # P-value

#----------------------------------------------
#Example 9.12
#Fatalities data
glm(Alcohol ~ Age, data = Fatalities, family = binomial)
f <- function(x){exp(-0.123-0.029*x)/(1+exp(-0.123-0.029*x))}

ggplot(Fatalities, aes(x = Age, y = Alcohol)) + geom_point() +
  stat_function(fun = f)

#alternative way to define f
f <- function(x){plogis(-0.123 - 0.029*x)}

#------------------------------------------
#Section 9.6
#Inference for logistic regression
fit <- glm(Alcohol ~ Age, data = Fatalities,
           family = binomial)
data.class(fit)  # is a "glm" object, so for help use:
help(glm)

fit          # prints the coefficients and other basic info
coef(fit)    # the coefficients as a vector
summary(fit) # gives standard errors for coefficients, etc.


# Full bootstrap - slope coeff. and prediction at age 20
N <- 10^3
n <- nrow(Fatalities)              # number of observations
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

quantile(beta.boot, c(.025, .975))      # 95% percentile CI
df <- data.frame(alpha.boot, beta.boot, pPred.boot)
ggplot(df, aes(x = beta.boot)) +
  geom_histogram(bins = 20, color = "white")
ggplot(df, aes(sample = beta.boot)) + geom_qq() + geom_qq_line()

