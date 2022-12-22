#------------------------------------------------
#Chapter 10 Categorical data
#Here is a function that computes the chi-square
#test statistic

#This function is a bit more enhanced than the code in the textbook
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
#Uncomment below if you haven't imported GSS2002 yet.
#GSS2002 <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/GSS2002.csv")

Education <- GSS2002$Education
DeathPenalty <- GSS2002$DeathPenalty
#Alternatively
#Education <- subset(GSS2002, select=Education, drop = TRUE)
#DeathPenalty <- subset(GSS2002, select=DeathPenalty, drop = TRUE)

table(Education, DeathPenalty)  #note education ordered alphabetically

Education <- ordered(GSS2002$Education,
                     levels = c("Left HS", "HS", "Jr Col", "Bachelors",
                                "Graduate"))

table(Education, DeathPenalty)

#Use function created above to calculate chi-square test statistic
observedChi2 <- chisq(table(Education, DeathPenalty))
observedChi2

#Find those rows where there is at least one NA
index <- which(is.na(Education) | is.na(DeathPenalty))

#Remove those rows from the two variables and define Educ2 and
#DeathPenalty2 to be the new vectors with those rows removed
Educ2 <- Education[-index]
DeathPenalty2 <-  DeathPenalty[-index]

N <- 10^4-1
result<-numeric(N)

for (i in 1:N)
 {
   DP.permutation <-sample(DeathPenalty2)
   GSS.table <- table(Educ2, DP.permutation)
   result[i]<-chisq(GSS.table, print = FALSE)
 }

#Create a histogram
 hist(result, xlab = "chi-square statistic", main = "Distribution of chi-square statistic")
 abline(v = observedChi2, col = "blue", lty = 5)


#optional: Create a histogram with the density curve
#imposed onto the histogram
#The prob=TRUE option below scales the histogram to have area 1
 hist(result, xlab = "chi-square statistic", main="Distribution of chi-square statistic",
    ylim = c(0,.2))
 curve(dchisq(x, df = 4), from = 0, to = 25, col = "green", add = TRUE)

#Compute P-value
(sum(result >= observedChi2) + 1)/(N + 1)


chisq.test(Education, DeathPenalty, simulate.p.value = TRUE, B = 10^4 - 1)
mat <- table(Education, DeathPenalty)
chisq.test(mat, simulate.p.value = TRUE, B = 10^4-1)

#----------------------------------------------------------------
#Example 10.2
mat <- rbind(c(42, 50), c(30, 87))
chisq.test(mat)

#Section 10.3.3 Fisher's Exact Test
fisher.test(mat)



#Section 10.4 Test of Homogeneity
candy.mat <- rbind(c(42, 20, 38), c(33, 27, 50))
candy.mat

chisq.test(candy.mat)

#Section 10.6
Phillies2009 <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Phillies2009.csv")
Homeruns <- Phillies2009$Homeruns
#Homeruns <- subset(Phillies2009, select = Homeruns, drop = TRUE)



lambda <- mean(Homeruns)
dpois(0:5, lambda)
table(Homeruns)

table(Homeruns)/162
