#------------------------------------------------
#Chapter 10 Categorical data
#Implementation of the permutation test of independence
#This function computes the chi-square
#test statistic

#
chisq <- function(observed, print = TRUE) {
  # Chi-square statistic for independence in a contingency table,
  # with related data exploration.
  # observed is the observed contingency table

  observedWithTotals <- cbind(observed, total = rowSums(observed))
  observedWithTotals <- rbind(observedWithTotals, total = colSums(observedWithTotals))
  expected <- outer(rowSums(observed), colSums(observed)) / sum(observed)
  statistic <- sum((observed-expected)^2/expected)
  if (print)
  {
  cat("Observed, with totals:\n")
  print(observedWithTotals)
  cat("\nRow Fractions:\n")
  print(round(observed / rowSums(observed), 3))
  cat("\nColumn Fractions:\n")
  print(round(observed / rep(colSums(observed), each = nrow(observed)), 3))

  cat("\nExpected:\n")
  print(round(expected, 1))
  cat("\nDifference:\n")
  print(round(observed - expected, 1))

  cat("\nChi-squared statistic for independence:", round(statistic, 1), "\n")
  }
  return(invisible(statistic))
}

#-------------------------------------------

#We use this function on the contingency table for Education and
#DeathPenalty
#set.seed(200)
library(resampledata3)
observed <- chisq(table(GSS2018$Degree, GSS2018$DeathPenalty))
observed 

#Now, there were 155 people who declined to respond to the
#death penalty question, so we will remove these observations from our
#analysis.

#We will use the drop_na() command from the tidyr package. The
#command below with create a data frame with variables Degree and DeathPenalty,
#removing any rows with an NA in either variable (though in this case, only 
#the death penalty variable has missing values.
                                                   
library(tidyr)
df <- drop_na(GSS2018, Degree, DeathPenalty)
#The sample(df$DeathPenalty) command below permutes the
#values in DeathPenalty
N <- 10^5-1
result <- numeric(N)
for (i in 1:N)
 {
   DP.permuted <- sample(df$DeathPenalty)
   GSS.table <- table(df$Degree, DP.permuted)
  result[i] <- chisq(GSS.table)
  }
                                                       
  ggplot() +  geom_histogram(aes(x = result)) +
  geom_vline(xintercept = observed, lty = 2)

#Check the distribution of the test statistics to help in determining
#the direction of the inequality when computing the $P$-value.

