##Wind speed case study
#Code for Wind speed case study which appears in
#Chapter 06 Estimation; 
#10 Categorical data; 
#13 Additional Topics

library(resampledata3)
library(ggplot2)
library(dplyr)
library(gridExtra)

weibull.shape <- function(k, data) {
  # This function computes the derivative of the log-likelihood for
  # the Weibull distribution with respect to the shape parameter k
  # (divided by n).  Set this equal to zero and solve to estimate the shape.
  numer <- colSums(outer(data, k, "^") * log(data))
  denom <- colSums(outer(data, k, "^"))
  numer/denom - 1/k - mean(log(data))
}

weibull.scale <- function(k, data) {
  # Maximum likelihood estimate for the Weibull scale parameter,
  # given shape parameter k
  mean(data^k)^(1/k)
}

# uniroot is a built-in R function which estimates the root of a function.
# Provide function, any arguments needed for function
# and a guess of two x values that surround the root; y must have opposite
# signs at the two guesses.


#First few rows of Turbine data set
head(Turbine)

wind <- Turbine$AveSpeed

uniroot(weibull.shape, data=wind, lower=1, upper=5)
# 3.169

##With estimate of shape parameter, now find estimate
##of scale parameters
weibull.scale(3.169, wind)
# 7.661

##To plot histogram with density overlap

df <- data.frame(wind)
df2 <- data.frame(x = c(2,14))

#----------------
# Figure 6.4a - hist & density
ggplot(df, aes(x = wind))  +
  geom_histogram(aes(y = stat(density)), bins = 10, color = "white") +
  stat_function(fun = dweibull, args = list(shape = 3.169, scale = 7.661)) +
  scale_x_continuous(breaks = seq(2,14, by = 2))+
  labs(x = "(m/s)", y = "Density")

#----------------
# Figure 6.4b - ecdf

# Theta = P(wind > 5), nonparametric and parametric estimates
n <- length(wind)
theta1 <- mean(wind > 5)
theta2 <- pweibull(5, 3.169, 7.661, lower.tail = FALSE)
theta1 # 0.75
theta2 # 0.7720827
eta1 <- quantile(wind, .1)
eta2 <- qweibull(.1, 3.169, 7.661)
eta1 # 3.77
eta2 # 3.766038


ggplot(df, aes(x = wind)) + stat_ecdf(geom="point") +
  stat_function(fun = pweibull, args = list(shape = 3.169, scale = 7.661)) +
  geom_vline(xintercept = 5) +
  annotate("segment", x = 3.7,xend = 4.7, y = 1-theta1, yend=1-theta1,
           arrow = arrow(length = unit(.1, "in")), color = "blue")+
  annotate("segment", x = 6.1,xend = 5.1, y = 1-theta2, yend=1-theta2,
           arrow = arrow(length = unit(.1, "in")), color = "blue") +
  annotate("text", x = 7, y = 1-theta2, parse = TRUE, label ="1-hat(theta)[2]" ) +
  annotate("text", x = 8, y = 1-theta2-.05, label = "Weilbull probability")+
  annotate("text", x = 3., y = 1-theta1, parse = TRUE, label ="1-hat(theta)[1]" ) +
  annotate("text", x = 3., y = 1-theta1+.1, label = "Empirical\n probability") +
  scale_x_continuous(breaks = seq(2,14, by = 2)) +
  labs(x = "(m/s)", y = "CDF")

#--------------------------------------------------------
##Chapter 10.5 Goodness of fit
##Example 10.8
##Get the deciles for Weibull distribution
quants <- qweibull(seq(0, 1, by=.1), 3.169, 7.661)
quants[11] <- 15  #since quants[11] = Infinity

#Get the counts in each sub-interval
hist(wind, breaks=quants, plot=F)$counts

count <- hist(wind, breaks=quants, plot=F)$counts
expected <- length(wind)*.1

##compute chi-square test statistic
sum ((count-expected)^2/expected)

#------------------------------------------------------------
#Chapter 13 Additional Topics

weibull.shape <- function(k, data) {
  # This function computes the derivative of the log-likelihood for
  # the Weibull distribution with respect to the shape parameter k
  # (divided by n).  Set this equal to zero and solve to estimate the shape.
  numer <- colSums(outer(data, k, "^") * log(data))
  denom <- colSums(outer(data, k, "^"))
  numer/denom - 1/k - mean(log(data))
}

weibull.scale <- function(k, data) {
  # Maximum likelihood estimate for the Weibull scale parameter,
  # given shape parameter k
  mean(data^k)^(1/k)
}


##-------------------------------------------
##Chapter 13 Additional Topics
##Section 13.2 Parametric bootstrap 
##Example 13.1
#Figure 13.4
ggplot(Turbine, aes(x = AveSpeed)) + stat_ecdf(geom="point") +
  stat_function(fun = pweibull, args = list(scale = 7.661, shape = 3.169)) +
  geom_hline(yintercept =0.1) +
  geom_hline(yintercept = 1, lty = 4, color = "gray") +
  geom_hline(yintercept = 0, lty = 4, color = "gray") +
  labs(x = "Wind (ms/)", y = "CDF")

#Bootstrapping quantiles wind speed
# Theta = P(wind > 5), nonparametric and parametric estimates
n <- length(wind)
theta1 <- mean(wind > 5)
theta2 <- pweibull(5, 3.169, 7.661, lower.tail = FALSE)
theta1 # 0.75
theta2 # 0.7720827
eta1 <- quantile(wind, .1)
eta2 <- qweibull(.1, 3.169, 7.661)
eta1 # 3.77
eta2 # 3.766038

# nonparametric bootstrap to find standard errors
set.seed(23)
B <- 10^4
boot.theta1 <- numeric(B)
boot.shape <-  numeric(B)
boot.scale <-  numeric(B)
boot.theta2 <- numeric(B)
boot.eta1 <- numeric(B)
boot.eta2 <- numeric(B)
for (i in 1:B)
{
  boot.wind <- wind[sample(1:n, replace=TRUE)]
  boot.theta1[i] <- mean(boot.wind > 5)
  boot.shape[i] <- uniroot(weibull.shape, data=boot.wind, lower=1,upper=5)$root
  boot.scale[i] <- weibull.scale(boot.shape[i], boot.wind)
  boot.theta2[i] <- pweibull(5, boot.shape[i], boot.scale[i], lower.t = FALSE)
  boot.eta1[i] <- quantile(boot.wind, .1)
  boot.eta2[i] <- qweibull(.1, boot.shape[i], boot.scale[i])
}

qqnorm(boot.theta1) # discrete, normal
qqnorm(boot.theta2) # continuous, normal
qqnorm(boot.eta1) # discrete, irregular, roughly normal
qqnorm(boot.eta2) # continuous, very normal

sd(boot.theta1) # 0.03331767
sd(boot.theta2) # 0.02342771
var(boot.theta1) / var(boot.theta2) # 2.022504

# Formula standard error, for comparison
sqrt(theta1 * (1-theta1) / n) # standard error = 0.03340766

sd(boot.eta1) # 0.2161054
sd(boot.eta2) # 0.1839509
var(boot.eta1) / var(boot.eta2) # 1.380154


# parametric bootstrap to find standard errors
set.seed(23)
pboot.theta1 <- numeric(B)
pboot.shape <-  numeric(B)
pboot.scale <-  numeric(B)
pboot.theta2 <- numeric(B)
pboot.eta1 <- numeric(B)
pboot.eta2 <- numeric(B)
for (i in 1:B)
{
  boot.wind <- rweibull(n, 3.169, 7.661)
  pboot.theta1[i] <- mean(boot.wind > 5)
  pboot.shape[i] <- uniroot(weibull.shape, data=boot.wind, lower=1,upper=5)$root
  pboot.scale[i] <- weibull.scale(pboot.shape[i], boot.wind)
  pboot.theta2[i] <- pweibull(5, pboot.shape[i], pboot.scale[i], lower.t=FALSE)
  pboot.eta1[i] <- quantile(boot.wind, .1)
  pboot.eta2[i] <- qweibull(.1, pboot.shape[i], pboot.scale[i])
}
etaRange <- range(boot.eta1, boot.eta2, pboot.eta1, pboot.eta2)

qqnorm(pboot.theta1) # discrete, slight neg skewness
qqnorm(pboot.theta2) # continuous, normal
qqnorm(pboot.eta1) # very normal, continuous
qqnorm(pboot.eta2) # very normal, continuous

sd(pboot.theta1) # 0.03198990
sd(pboot.theta2) # 0.02604409
var(pboot.theta1) / var(pboot.theta2) # 1.508716
# Similar to the nonparametric bootstrap, though relative efficiency differs

sd(pboot.eta1) # 0.2823103
sd(pboot.eta2) # 0.2100147
var(pboot.eta1) / var(pboot.eta2) # 1.806982

#----------------
#Figure 13.5(a)
df <- data.frame(boot.eta1, pboot.eta1)
p1 <- ggplot(df, aes(sample = boot.eta1)) + geom_qq(size = .5, aes(color = "A")) +
  labs(y = "Estimator is ordinary quantile\n Bootstrap statistic", title = "(a)") +
  ylim(c(3,5))+
  scale_color_manual(name = NULL, values = c("A" = "black"),
                     labels = c("Ordinary bootstrap"),
                     guide = guide_legend(label.position = "left",
                                          override.aes = list(shape = "")))+
  theme(legend.position = c(.3,.9),
        legend.title=element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = rel(.8)),
        plot.margin = margin(5.5,0,5.5,9, "pt"),
        axis.title = element_text(size = rel(.8)))

p2 <- ggplot(df, aes(sample = boot.eta2)) + geom_qq(size = .5, aes(color = "A")) +
  ggtitle("(b)") + ylim(c(3,5))+
  scale_color_manual(name = NULL, values = c("A" = "black"),
                     labels = c("Parametric (Weibull) bootstrap"),
                     guide = guide_legend(label.position = "left",
                                          override.aes = list(shape = "")))+
  theme(legend.position = c(.4,.9),
        legend.title=element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = rel(.8)),
        axis.title.y = element_blank(),
        axis.title = element_text(size = rel(.8)),
        plot.margin = margin(5.5,0,5.5,2, "pt"))

  grid.arrange(p1, p2, ncol = 2)

#---------------------------------
#Figure 13.6 Estimator is weibull quantile
df2 <- data.frame(boot.eta2, pboot.eta2)
p3<-ggplot(df, aes(sample = boot.eta2)) + geom_qq(size = .5, aes(color = "A")) +
  labs(y = "Estimator is Weibull quantile\n Bootstrap statistic", title = "(a)") +
  ylim(c(2.8, 4.8)) +
  scale_color_manual(name = NULL, values = c("A" = "black"),
                     labels = c("Ordinary bootstrap"),
                     guide = guide_legend(label.position = "left",
                                          override.aes = list(shape = "")))+
  theme(legend.position = c(.3,.9),
        legend.title=element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = rel(.8)),
        plot.margin = margin(5.5,0,5.5,9, "pt"),
        axis.title = element_text(size = rel(.8)))

p4 <- ggplot(df2, aes(sample = pboot.eta2)) +
  geom_qq(size = .5, aes(color = "A")) +
  labs(y = "Bootstrap statistic", title = "(b)") +
  ylim(c(2.8,4.8))+
  scale_color_manual(name = NULL, values = c("A" = "black"),
                     labels = c("Parametric (Weibull) bootstrap"),
                     guide = guide_legend(label.position = "left",
                                          override.aes = list(shape = "")))+
  theme(legend.position = c(.35,.9),
        legend.title=element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = rel(.8)),
        axis.title.y = element_blank(),
        axis.title = element_text(size = rel(.8)),
        plot.margin = margin(5.5,0,5.5,2, "pt"))

grid.arrange(p3, p4, ncol=2)
