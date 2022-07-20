#Chapter 2 EDA Edition 3

# Type commands after the prompt $>$ and
# then press the  $<$ENTER$>$  key.

# Note: anything after the pound symbol \# is a
# comment---explanatory text that is not executed.

4*9           # Simple arithmetic

# Create a sequence incrementing by $1$:

20:30


# We will create an object called dog and assign it the values
# 1, 2, 3, 4, 5. The symbol <- is the assignment operator.

dog <- 1:5
dog

dog + 10

3*dog

sum(dog)        # 1+2+3+4+5

# The object dog is called a vector.

# If you need to abort a command, press the escape key  <ESC>.
# The up arrow key uparrow can be used to recall previous entries.

# To obtain help on any of the commands,  type the
# name of the command you wish help on:
  
?sum

### Importing data

# Data for the third edition of the textbook can be downloaded from the 
# web site (http://github.com/chihara/MathStatsResamplingR/Edition3/Data) or by 
# installing the R package **resampledata3**.

# For instance, let's start with the Flight Delays Case Study
# (see Chapter 1, **Case Studies**), of the text for a description of this
# data set).  We use the  read.csv  command to import the data into our R workspace:

library(resampledata3)  

# Alternatively, if the data set has been downloaded to the working directory
# FlightDelays<- read.csv("FlightDelays.csv")
# View(FlightDelays)

#The str command gives a compact display of the internal structure of the data frame:

str(FlightDelays)

#Alternatively, the summary gives numeric summaries of the variables:

summary(FlightDelays)

# Other commands to get basic information about the data frame include names, 
# to view the names of the variables in FlightDelays:

names(FlightDelays)

#To view the first 6 rows of the data, use the head command:

head(FlightDelays)     #default, 6 rows

head(FlightDelays, 4)  #first 10 rows

# (What do you suppose the tail command does?)

# To check the  size (number of rows and columns) of the data frame, type

dim(FlightDelays)   # dim = dimension

# This tells us that there are 4029 observations and 10 columns.

### Tables, bar charts and histograms

# The factor variable Carrier in the FlightDelays data set assigns
# each flight to one of two  *levels*: UA or AA. To obtain the summary of this variable, we
# use the $ operator to extract this variable from the FlightDelays data frame:

table(FlightDelays$Carrier)

#### Remark

# R is **case-sensitive**! Carrier and carrier are considered different.

# To create a contingency table summarizing the relationship between
# carrier ( Carrier ) and whether or not a flight was delayed by more than 30 minutes ( Delayed30 ), type:


table(FlightDelays$Carrier, FlightDelays$Delayed30)

#To perform additional operations, we will first store the table object:

out <- table(FlightDelays$Carrier, FlightDelays$Delayed30)
out

addmargins(out)  # table with row/column sums 


# The proportions command gives joint or conditional distributions:


proportions(out)           #joint distribution (sum of all cells = 1)

proportions(out, 1)        #conditional distributions (row sums = 1)

# Thus, 9.8% of flights were American Airlines and delayed by more than 30
# minutes, whereas of all American Airline flights, 13.5% were
# delayed by more than 30 minutes

# To visualize the distribution of a factor variable, create a *bar chart*:

barplot(table(FlightDelays$Carrier))

# The barplot command is part of base R, that is, it comes with the default 
# installation of R. Researchers have developed *packages* that enhance 
# many of the basic R commands. One such package is the **ggplot2** package.

library(ggplot2)

ggplot(FlightDelays, aes(x = Carrier)) + geom_bar()
 
# Note that the syntax is different from the base R 
# command for a bar plot: in particular, we do not first have to create a table.

# To visualize the distribution of a numeric variable, create a histogram.

hist(FlightDelays$Delay)    # base R

ggplot(FlightDelays, aes(x = Delay)) + geom_histogram()   # ggplot2
 
# The shape of the distribution of this variable is *right-skewed*.

# The default number of bins for the **ggplot2** histogram is 30. We'll change this here:
  
ggplot(FlightDelays, aes(x = Delay)) + geom_histogram(bins = 8, color = "white")

### Numeric Summaries

# Because it is a bit cumbersome to use the syntax  FlightDelays$Delay 
# each time we want to work with the  Delay  variable, we will extract this variable
# and store it in the (vector) object  delay :
  

delay <- FlightDelays$Delay
head(delay)
mean(delay)
mean(delay, trim = .05)   #trimmed mean
median(delay)
 
# Other basic statistics:
 
max(delay)
min(delay)
range(delay)
var(delay)           #variance
sd(delay)            #standard deviation
quantile(delay)      #quartiles
 
# If you need the population variance (that is, denominator of 1/n instead of
# 1/(n-1)),

n <- length(delay)
(n-1)/n*var(delay)
 
### Statistics with grouping by a categorical variable

# To find statistics for a numeric variable grouped by a 
# categorical variable, one option is to use the  tapply  command

tapply(FlightDelays$Delay, FlightDelays$Carrier, mean)
tapply(FlightDelays$Delay, FlightDelays$Carrier, sd)
 
# Thus, the mean length of an American Airlines flight delay was 10.1 
# minutes compared to 15.98 minutes for United Airlines. 
# The standard deviations for AA and UA were 40.08 m and 45.1 m, respectively.

# Another approach is to use the commands in the **dplyr** package

library(dplyr)
FlightDelays %>% group_by(Carrier) %>% summarize(mean(Delay), sd(Delay))
 
# The %>% is the piping operator in the **dplyr** package: it takes the data set  
# FlightDelays  and
# “pipes” it to the command group by. The  group_by  command groups the observations 
# by  Carrier, that is by AA or UA. Then we pipe this information to the 
# summarize command and compute the
# mean and standard deviation of the  Delay  variable for each of AA and UA flights.

### Boxplots

# Boxplots give a visualization of the 5-number summary of a variable.

summary(delay)         #numeric summary
boxplot(delay)
 
# To compare the distribution of a numeric variable across the levels of
# a categorical (factor) variable:
  
boxplot(Delay ~ Day, data = FlightDelays)           # base R

ggplot(FlightDelays, aes(x = Day, y = Delay)) + geom_boxplot()  # ggplot2

### Normal quantile plots

# We create normal quantile plots to see if it is plausible that 
# data come from a normal distribution. 
# The  ggplot  command requires a data frame for it's first argument.
 
x <- c(17.7, 22.6, 26.1, 28.3, 30, 31.2, 31.5, 33.5, 34.7, 36)
df <- data.frame(x = x)
ggplot(df, aes(sample = x)) + geom_qq() + geom_qq_line() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles")

ggplot(NCBirths2004, aes(sample = Weight))+ geom_qq() +
  geom_qq_line()

ggplot(NCBirths2004, aes(sample = Weight)) + geom_qq() +
  geom_qq_line() + facet_grid( ~ Smoker)
 

# Let's generate a random sample of size 100 from the normal distribution N(30, 6) and
# create the normal-quantile plot. 

my.sample <- rnorm(100, 30, 6)
df <- data.frame(my.sample)     #create data frame
ggplot(df, aes(sample = my.sample)) + geom_qq() + geom_qq_line()

### Empirical Cumulative Distribution Function (ECDF)

x <- c(3, 6, 15, 15, 17, 19, 24)
df <- data.frame(x = x)
ggplot(df, aes(x = x)) + stat_ecdf()

df <- data.frame(x = rnorm(25))     # random sample from N(0,1)
ggplot(df, aes(x = x)) + stat_ecdf() +
  stat_function(fun = pnorm, color = "blue") +
  labs(x="", y = "F(x)")

# Using the data from the Beer and Hotwings case study:

ggplot(Beerwings, aes(x = Beer, linetype = Gender, color = Gender)) +
  stat_ecdf() +  labs(x = "Beer (oz)", y = "F(x)") +
  geom_vline(xintercept = 25, lty = 3) 

### Scatter plots

ggplot(Beerwings, aes(x = Hotwings, y = Beer)) +
  geom_point()

#We can also distinguish the two genders by adding the color
# aesthetic:

ggplot(Beerwings, aes(x = Hotwings, y = Beer,
                      color = Gender)) + geom_point()

### Misc. Remarks

# Functions in R are called by typing their name followed by arguments surrounded
# by *parentheses*: ex.  hist(delay) . Typing a function name without parentheses
# will give the code for the function.

sd

# We saw earlier that we can assign names to data (we created a vector called  dog .)
# Names can be any length, must start with a letter, and may contain letters or numbers:
  
fish25 <- 10:35
fish25
 
# Certain names are **reserved** so be careful to not use them:  cat ,  c ,   t ,  T ,  F ,...

# To be safe, before making an assignment, type the name:

whale
 
# If you get the output  Problem: Object "whale" not found , then it is safe to use  whale !
  
whale <- 200
objects()
rm(whale)
objects()
 
# In general, R is space-insensitive.

3 +4
3+   4
mean(3+5)
mean ( 3 + 5 )
 
# BUT, the assignment operator must not have spaces!
#   $<$-  is different from  $<$ \, -  
  
#  To quit, type  q()  at the prompt.

# You will be given an option to save the workspace.

# If you select  **Yes**:
# all objects created in this session are saved to your working directory so
# that the next time you start up  R , if you load this working directory,
# these objects will still be available.
# You will not have to re-import  FlightDelays , for instance, nor recreate
# delay .

# You can, for back-up purposes, save  data to an external file/disk by using,
# for instance, the   write.csv  command. See the help file for more information.

