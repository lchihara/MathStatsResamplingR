#Chapter 10
#Categorical data
library(resampledata3)
library(ggplot2)
library(dplyr)

#Section 10.2 Permutation Test of Independence
chisq.test(GSS2018$Degree, GSS2018$DeathPenalty, simulate.p.value = TRUE, B = 10^5-1)
mat <- table(GSS2018$Degree, GSS2018$DeathPenalty)
chisq.test(mat, simulate.p.value = TRUE, B = 10^5-1)

#Section 10.3
1 - pchisq(50.449, 4)

chisq.test(GSS2018$Degree, GSS2018$DeathPenalty)

mat <- rbind(c(42, 50), c(30, 87))
chisq.test(mat)
fisher.test(mat)

#Section 10.4 Test of Homogeneity
candy.mat <- rbind(c(42, 20, 38), c(33, 27, 50))
candy.mat

chisq.test(candy.mat)

#Section 10.5
qchisq(c(.2, .4, .6, .8), 10)


Homeruns <- Phillies2009$Homeruns

lambda <- mean(Homeruns)
dpois(0:4, lambda)
table(Homeruns)

table(Homeruns)/162
