#Chapter 2 EDA Edition 3

library(resampledata3)
library(dplyr)
library(ggplot2)


#Barchart
ggplot(FlightDelays, aes(x = Carrier)) + geom_bar()


#Histogram
#Need to get United Airlines
UA <- FlightDelays %>% filter(Carrier == "UA")
ggplot(UA, aes(x = Delay)) + geom_histogram(color = "white",binwidth=50, center = 25) + xlab("Time (min)")


janTemp <- read.csv("02janTemps.csv")
ggplot(janTemp, aes(x = Washington)) + geom_histogram(color = "white",binwidth=5, boundary = 40) + xlab("Temperature (F)")


#Dotplot
data <- data.frame(x = c(4, 4.5, 4.5, 5, 5, 5, 6, 6, 6.5, 7, 7, 7))
ggplot(data, aes(x = x)) + geom_dotplot(dotsize = .7,stackratio = 1.5)+scale_y_continuous(NULL, breaks = NULL) +
  coord_fixed(ratio = 1) + scale_x_continuous(breaks = seq(4,7, by = .5), labels =
                      as.character(seq(4,7,by = .5))) + xlab("")


#boxplot
data <- data.frame(y = c(5, 6, 6, 8, 9, 11, 11, 14, 17, 17, 19, 20, 21, 21,
              22, 23, 24, 32, 40, 43, 49))
ggplot(data, aes(y=y, x = 1)) + geom_boxplot() + labs(x="",y="") +
  scale_x_continuous(breaks = NULL)


UA$Day2 <- factor(UA$Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
ggplot(UA, aes(x = Day2, y = Delay)) + geom_boxplot() + labs(x = "Day", y = "Delay (m)")


#histogram
NCBirths2004 <- read.csv("C:\\Users\\lchihara\\MathStats-Textbook\\trunk\\data\\NCBirths2004.csv")
ggplot(NCBirths2004, aes(x = Weight))  + geom_histogram(color = "white", boundary=1500,binwidth=250) +
    labs(x = "Weight (g)")


#Density
#limitRange from R Graphics Cookbook
limitRange <- function(mean,sd, min, max){
  function(x){
    y <- dnorm(x, mean, sd)
    y[x < min | x > max] <- NA
    return(y)
  }
}


p <- ggplot(data.frame(x = c(-25, 25)), aes(x=x))
p + stat_function(fun = limitRange(3, 5, -25, 4.27), geom = "area", fill = "steelblue3")+
  stat_function(fun = dnorm, args=list(mean = 3, sd = 5)) +
  labs(x="", y = "Density")


#QQ-plot
x <- c(17.7, 22.6, 26.1, 28.3, 30, 31.2, 31.5, 33.5, 34.7, 36)
ggplot() + geom_qq(aes(sample=x)) + geom_qq_line(aes(sample=x))


ggplot(NCBirths2004, aes(sample = Weight))+ geom_qq() + geom_qq_line()

## This uses qqhist() defined above by source("qqhist.R")
#Grab random sample of the delay times
set.seed(24)
x <- sample(UA$Delay, 100, replace = FALSE)

qqhist(x, yname="Delay times")
})


x <- janTemp$Washington

qqhist(x, yname = "January temperatures")
})


#Heavy tails
set.seed(398)
x <- rt(150, 3)
data <- data.frame(x)
p <- ggplot(data, aes(x=x)) +
  geom_histogram(aes(y=..density..),binwidth=1, center=.5,color = "white")
p
p + stat_function(fun= dnorm, args=list(mean=0, sd = 1)) +
   labs(x = "Heavy tailed example")


ggplot(data, aes(sample = x)) + geom_qq() + geom_qq_line()


##ECDF
data <- data.frame(x=c(3, 6, 15, 15, 17, 19, 24))
ggplot(data, aes(x=x)) + stat_ecdf() + xlab("") + ylab("F(x)")

#Alternative syntax
#x <- c(3, 6, 15, 15, 17, 19, 24)
#ggplot() + stat_ecdf(aes(x=x)) + labs(x="", y = expression(F[n](x)))

set.seed(666)
data  <- data.frame(x=rnorm(25))
ggplot(data, aes(x=x)) + stat_ecdf() + stat_function(fun=pnorm, color = "blue") +
  labs(x="", y = "F(x)")


ggplot(Beerwings,aes(x = Beer, linetype = Gender, color = Gender)) + stat_ecdf() +
  labs(x="Beer (oz)", y = "F(x)") + geom_vline(xintercept = 25, lty = 3) +
  scale_linetype_manual(values=c(1,5))


##Scatter plot
ggplot(Beerwings, aes(x = Hotwings, y = Beer)) + geom_point()



library(gridExtra)
EDAscatter <- read.csv("02EDAscatter.csv")

p <- ggplot(EDAscatter, aes(x = x, y = y)) + geom_point() + labs(x = "", y = "") +
       scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
      ggtitle("Positive, linear, strong") +
      theme(plot.title = element_text(size = rel(.8), hjust = .5))

p2 <- ggplot(EDAscatter, aes(x = x2, y = y2)) + geom_point() + labs(x = "", y = "") +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL)  +
  ggtitle("Negative, linear, moderate") +
  theme(plot.title = element_text(hjust = .5,size = rel(.8)))


p3 <- ggplot(EDAscatter, aes(x = x3, y = y3)) + geom_point() + labs(x = "", y = "") +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL)+
  ggtitle("Negative, linear, weak")+
  theme(plot.title = element_text(size = rel(.8), hjust = .5))


p4 <- ggplot(EDAscatter, aes(x = x4, y = y4)) + geom_point() + labs(x = "", y = "") +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  ggtitle("Curved, moderate")+
  theme(plot.title = element_text(size = rel(.8), hjust = .5))

grid.arrange(p, p2, p3, p4, ncol = 2)



##Skewness and kurtosis
p <- ggplot(data.frame(x = c(-3,3)), aes(x = x)) + stat_function(fun = dnorm) +
          ggtitle("Skewness = 0, kurtosis = 0") + labs(x = "", y = "")+
         theme(plot.title = element_text(size = rel(.8), hjust = .5))
p2 <-  ggplot(data.frame(x = c(0, 40)), aes(x = x)) +
       stat_function(fun = dchisq, args = list(df = 20)) +
          ggtitle("Skewness = 0.63, kurtosis = .6") + labs(x = "", y = "")+
  theme(plot.title = element_text(size = rel(.8), hjust = .5))

p3 <-  ggplot(data.frame(x = c(0, 30)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = 10)) +
  ggtitle("Skewness = 0.89, kurtosis = 1.2") + labs(x= "", y = "")+
  theme(plot.title = element_text(size = rel(.8), hjust = .5))

p4 <- ggplot(data.frame(x = c(0, 20)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = 4)) +
  ggtitle("Skewness = 1.41, kurtosis = 3") + labs(x = "", y = "")+
  theme(plot.title = element_text(size = rel(.8), hjust = .5))


grid.arrange(p, p2, p3,p4, ncol = 2)


#Exercises
x <- c(1, 2, 5, 8, 8, 8, 10, 10, 14, 14.8, 20, 20, 20, 20, 21, 23, 25, 26, 26, 28)
ggplot() + stat_ecdf(aes(x)) + scale_x_continuous(breaks = seq(0, 30, by = 2)) +
   scale_y_continuous(breaks = seq(0, 1, by = .05)) +
   labs(y = expression(F(x))) +
      theme(panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())

