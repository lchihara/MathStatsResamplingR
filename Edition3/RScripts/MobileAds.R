##################################################
# Google Mobile Adds data

# There's a lot of exploratory data analysis here.
# For now I'm going to put the original plot ahead of all that;
# search for "Original Plot"

library(ggplot2)
library(gridExtra)
library(scales)
library(dplyr)

Table <- function(...) table(..., useNA = "ifany")

PlotSort <- function(x, ..., ylab = NULL, xlab = NULL){
  # Transposed plot of empirical CDF - data on y axis, quantiles on x axis
  # Args:
  #   x  : numerical vector
  #   ... : other arguments passed to 'plot'
  #   xlab : xlab, defaults to "Quantile"
  #   ylab : xlab, defaults to name of the variable 'x'
  if(anyNA(x))
    x <- x[!is.na(x)]
  if(is.null(ylab))
    ylab <- deparse(match.call()$x) # substitute gives numeric vector
  if(is.null(xlab))
    xlab <- "Quantile"
  plot(ppoints(x), sort(x), ..., ylab = ylab, xlab = xlab)
}

PointsSort <- function(x, ...){
  # Transposed plot of empirical CDF - data on y axis, quantiles on x axis
  # Like PlotSort, but add to previous plot
  # Args:
  #   x  : numerical vector
  #   ... : other arguments passed to 'points'
  if(anyNA(x))
    x <- x[!is.na(x)]
  points(ppoints(x), sort(x), ...)
}

PlotSortGroups <- function(x1, x2, ..., x3 = NULL, x4 = NULL,
                           ylab = NULL, xlab = NULL, ylim = NULL){
  # Inverse CDF plots for up to four variables
  # x1 can be a matrix with 2 to four columns, or the variables can
  # be given in individual columns (x3 and x4 must be given by name).
  if(is.matrix(x1)) {
    if(ncol(x1) >= 4)
      x4 <- x1[, 4]
    if(ncol(x1) >= 3)
      x3 <- x1[, 3]
    if(ncol(x1) >= 2)
      x2 <- x1[, 2]
    x1 <- x1[, 1]
  }
  if(is.null(ylim))
    ylim <- range(x1, x2, x3, x4, na.rm=TRUE)
  PlotSort(x1, ylim = ylim, ylab = ylab, xlab = xlab, ...)
  PointsSort(x2, col = 2, ...)
  if(!is.null(x3))
    PointsSort(x3, col = 3, ...)
  if(!is.null(x4))
    PointsSort(x4, col = 4, ...)
}

catn <- function(...) {
  # cat, but with a final newline.
  # catn("text") is equivalent to cat("text\n")
  if (any(nzchar(names(match.call())))) {
    # Quick and dirty way to handle file and append arguments;
    # this results in an extra " " before the final "\n".
    cat(..., "\n")
  } else {
    cat(...)
    cat("\n")
  }
}

log1 <- function(x) log(1+x)

#--------------------------------------------------
# load data
#exact path will vary depending on user set-up
#mobileAds <- read.csv("../Data/MobileAds.csv", stringsAsFactors = F,
#                row.names = 1)

#--------------------------------------------------
#### Variables

# impr_post    # impressions (not thousands)
# impr_pre

# click_post   # clicks (not thousands). click <= impr
# click_pre

# cost_post    # What they paid for the clicks.
# cost_pre

# conv_post    # Number of conversions.
# conv_pre

# value_post   # Value of the conversions.
# value_pre

# cpm_pre      # cpm = cost / impr, cost per impression
# cpm_post

# cpc_pre      # cpc = cost / click, cost per click
# cpc_post

# cpa_pre      # cpa = cost / conv (except is 0 if conv==0), cost per conversion
# cpa_post

# cpr_pre      # cpr = cost / value (except is 0 if value=0), cost per return
# cpr_post

# All of the above come in two versions:
# m.* (mobile)
# d.* (desktop)

# error.cpr_pre    # abs(m.cpr_pre - d.cpr_pre)
# error.cpr_post   # abs(m.cpr_post - d.cpr_post)

# mult.change  # Change in mobile multiplier between pre and post

# The original plot was for error.cpr_pre and error.cpr_post

# Summary of variables:
#  impr = impressions, click, cost, conv = conversions, value
#  impr -> (click & cost) -> conversions -> value
#  cpm = cost / impr = cost per impression      (or 0 if 0/0)
#  cpc = cost / click = cost per click          (or 0 if 0/0)
#  cpa = cost / conv = cost per conversion      (or 0 if denominator 0)
#  cpr = cost / value = cost per return = 1/ROI (or 0 if denominator 0)
#    All above have _pre _post,
#    and have m. (mobile) and d. (desktop)
#  error_{pre,post} = abs(m.cpr - d.cpr)

# Trying to get parity between mobile & desktop
# Error = | distance between mobile and desktop/tablet |
# Want smaller error.
# data is for one advertiser
# CPR = cost per ($1) return. Inverse of ROAS (return-on-ad-spend)

# d. = computer/tablet (synonymous to 'desktop') which refers to outside of mobile.
# m. = mobile
# This is needed because mobile CPR is compared against non-mobile CPR to define how much apart mobile's willingness to pay is compared to that of non-mobile.

# The problems was around advertisers' mobile bids. With the launch of AdWords "Enhanced Campaigns" with specific mobile bid multipliers that now has to be applied on top of desktop campaigns' max CPC bids, a lot of advertisers didn't understand how to bid appropriately on mobile. Although the "Enhanced Campaign" was launched to simplify the campaign management for advertisers, it was initially perceived to provide less control to advertisers by not being able to have separate campaigns for device types (e.g. mobile specific campaign, desktop specific campaign, tablet specific campaign, etc.).
#
# As a result, many auctions had advertiser surpluses (or unreached mobile traffic) because many advertisers were underbidding per click or conversion. In order to help with that, an experiment was crafted to see what the overall profitability will be if the willingness to pay per mobile $ conversion (or cost per revenue; CPR) were to that of desktop (i.e. mobile CPR = desktop CPR) - all assuming last click conversion model. Because at the end of the day, a $1 revenue on mobile is the same benefit to advertisers as a $1 revenue on desktop.
#
# The experiment was designed at the ad_group level where the mobile bid modifiers were set for this particular advertiser. In turn, this had to be a pre vs post paired t test. But given that like many things, there was a pareto effect reflected on the distribution of data where a few ad_groups had exhorbitant cost for its revenue return - while majority of them were in line with the desired cost per revenue (put it differently, return on ad spend).
#
# The initial results were mixed; but eventually, this started seeing positive conversion and profit lift for the advertisers by running this mobile bid strategy for a handful of campaigns that had stable volume of traffic.


#--------------------------------------------------
#### Original Plot
prepost <- ggplot(data = mobileAds) +
  geom_density(aes(x = error.cpr_pre, colour = "pre")) +
  geom_density(aes(x = error.cpr_post, colour = "post")) +
  xlab("CPR Error") +
  ylab("Distribution (Density)") + theme_bw() +
  ggtitle("Pre v Post") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.title = element_blank())
print(prepost)


# Do with base graphics instead - can't use color, + consistency with later plot

par(mex=.8, cex=.8)
with(mobileAds,
     plot(density(error.cpr_pre, to = max(error.cpr_pre, error.cpr_post)), main="",
          xlab = "CPR Error", "Distribution (Density)"))
with(mobileAds,
     lines(density(error.cpr_post), lty=2))
abline(h=0, v=0, col="gray")
legend("topright", lty=1:2, legend=c("pre", "post"))



# Another pplot for original measure - the error change
errorch <- ggplot(data = mobileAds) +
  geom_density(aes(x = error.cpr_post - error.cpr_pre), color = "red") +
  xlab("Error Change") +
  ylab("Distribution (Density)") + theme_bw() +
  ggtitle("Error Change") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, color = "dark gray", linetype="dotted")
grid.arrange(prepost, errorch, ncol=2)

#--------------------------------------------------
# Investigate variable names
basicVars <- c("impr", "click", "cost", "conv", "value", "cpm", "cpc", "cpa", "cpr")
temp <- c(t(outer(basicVars, c("_pre", "_post"), paste0)))
bothNames <- c(paste0("m.", temp),
               paste0("d.", temp))
all(bothNames %in% names(mobileAds)) # TRUE
setdiff(names(mobileAds), bothNames) # mult.change, error.cpr_{pre,post}

# Check for missing values
sapply(mobileAds, anyNA) # None
# Check for zeros
sapply(mobileAds[bothNames], min) == 0
# Quite a few, including m.impr_post and d.impr_post - those data not useful,
# I'll exclude them below to create mobileAds2

# Confirm relationships between basic variables
#   impr >= click
#   gz(click) >= gz(cost)   # greater than zero; i.e. cost>0 only if click>0
#   gz(click) >= gz(conv)   # can have multiple conversions per click
#   gz(conv) >= gz(value)
#   cpm = cost/impr or 0    # possibly 0/0 - should be NA
#   cpc = cost/click or 0   # possibly 0/0 - should be NA
#   cpa = cost/conv or 0    # possibly positive/0 - should be infinite
#   cpr = cost/value or 0   # possibly positive/0 - should be infinite
# For all of the cp* variables, if denominator is zero the cp* is zero.
# That is potentially misleading.

gz <- function(x) (x >= 0) # For checking if something is positive
Name <- function(x) paste0(prefix, x, suffix)
CheckEqual <- function(x, y, message) {
  temp <- all.equal(x, y)
  if(!identical(temp, TRUE)) {
    catn(message)
    print(temp)
  }
}
Divide <- function(x, y) ifelse(x == 0, 0, y/x)
for(prefix in c("m.", "d.")) {
  cat("\nprefix =", prefix)
  for(suffix in c("_pre", "_post")) {
    catn("\nsuffix =", suffix)
    impr <- mobileAds[[Name("impr")]]
    click <- mobileAds[[Name("click")]]
    cost <- mobileAds[[Name("cost")]]
    conv <- mobileAds[[Name("conv")]]
    value <- mobileAds[[Name("value")]]
    cpm <- mobileAds[[Name("cpm")]]
    cpc <- mobileAds[[Name("cpc")]]
    cpa <- mobileAds[[Name("cpa")]]
    cpr <- mobileAds[[Name("cpr")]]
    if(any(impr < click))         catn("impr < click")
    if(any(gz(click) < gz(cost))) catn("click=0, cost>0")
    if(any(gz(click) < gz(conv))) catn("click=0, conv>0")
    if(any(gz(conv) < gz(value))) catn("conv=0, value>0")
    CheckEqual(cpm, Divide(impr, cost), "cpm != cost/impr")
    CheckEqual(cpc, Divide(click, cost), "cpc != cost/click")
    CheckEqual(cpa, Divide(conv, cost), "cpa != cost/conv")
    CheckEqual(cpr, Divide(value, cost), "cpr != cost/value")
  }
}
# To look at the last set.
cbind(impr, click, cost, conv, value, cpm=round(cpm,2), cpc=round(cpc,2),
      cpa=round(cpa,2), cpr=round(cpr,2))
# Remove the variables so I don't accidentally use them below.
rm(impr, click, cost, conv, value, cpm, cpc, cpa, cpr)


# Some exploratory plots
with(mobileAds, plot(m.impr_pre, m.click_pre)); abline(0,1)
with(mobileAds, plot(m.impr_pre, m.cost_pre))
with(mobileAds, plot(m.click_pre, m.conv_pre)); abline(0,1)
with(mobileAds, plot(m.conv_pre, m.value_pre))
with(mobileAds, plot(m.impr_post, m.click_post)); abline(0,1)
with(mobileAds, plot(m.impr_post, m.cost_post))
with(mobileAds, plot(m.click_post, m.conv_post)); abline(0,1)
with(mobileAds, plot(m.conv_post, m.value_post))

# Relationships before and after
with(mobileAds, plot(m.impr_pre, m.impr_post)); abline(0,1) # weak corr, around line
with(mobileAds, plot(m.click_pre, m.click_post)); abline(0,1) # ditto
with(mobileAds, plot(m.cost_pre, m.cost_post)); abline(0,1) # ditto
with(mobileAds, plot(m.conv_pre, m.conv_post)); abline(0,1) # ditto
with(mobileAds, plot(m.value_pre, m.value_post)); abline(0,1) # ditto

pos <- function(x) ordered(ifelse(x == 0, "zero", "positive"), levels=c("zero", "positive"))
with(mobileAds, Table(pos(m.impr_pre), pos(m.click_pre))) # all positive
with(mobileAds, Table(pos(m.click_pre), pos(m.cost_pre))) # all positive
with(mobileAds, Table(pos(m.click_pre), pos(m.conv_pre))) # 22 w/o conversions
with(mobileAds, Table(pos(m.conv_pre), pos(m.value_pre))) # 61 w conversions with no value
with(mobileAds, Table(pos(m.cost_pre), pos(m.value_pre))) # 83 w cost but no value

with(mobileAds, Table(pos(m.impr_post), pos(m.click_post))) # 14 imp=0, +33 click=0
with(mobileAds, Table(pos(m.click_post), pos(m.cost_post))) # 47 with both zero
with(mobileAds, Table(pos(m.click_post), pos(m.conv_post))) # 47 click=0, +154 conv=0
with(mobileAds, Table(pos(m.cost_post), pos(m.conv_post)))  # 47 cost=0, +154 conv=0
with(mobileAds, Table(pos(m.conv_post), pos(m.value_post))) # 201 conv=0, +83 value=0
with(mobileAds, Table(pos(m.cost_post), pos(m.value_post))) # 47 cost=0, +237 value=0

# There are 47 cases with click=0, cost=0, then conv=0 & value=0
# They have cpa and cpr undefined (rather than 0).

# I think I should exclude them from the dataset. They essentially have
# no information for the post data.

with(mobileAds, Table(pos(d.cost_post), pos(d.conv_post)))
# 28 with cost=0 for ct; also exclude them.


# Plot same data as original plot, but
# use inverse CDF instead of density, and sqrt transformation.
PlotSortGroups(sqrt(mobileAds$error.cpr_pre), sqrt(mobileAds$error.cpr_post),
               ylab = "sqrt(error)")
legend("topleft", col=1:2, legend=c("error.cpr_pre", "error.cpr_post"), pch=1)
# Even with sqrt transformat, distributions have moderately long right tail.
# Post has many more zeros. But these may be misleading - where
# cpr is infinite but recorded as zero.


#--------------------------------------------------
# Exclude cases with no clicks
mobileAds2 <- subset(mobileAds,
                m.click_pre > 0 & d.click_pre > 0 &
                m.click_post > 0 & d.click_post > 0)
dim(mobileAds2)
# 590 rows, down from 655.

with(mobileAds2, Table(d.cpr_pre > 0)) # 89 501
with(mobileAds2, Table(d.cpr_post > 0)) # 126 464
# There are zeros, that should be infinite or undefined.

with(mobileAds2, plot(m.cpr_pre, m.cpr_post)) # many y=x, many x=0, many others
with(mobileAds2, plot(sqrt(m.cpr_pre), sqrt(m.cpr_post)))
with(mobileAds2, plot(sqrt(d.cpr_pre), sqrt(d.cpr_post)))
# ct has smaller range, fewer zeros
with(mobileAds2, PlotSortGroups(sqrt(m.cpr_pre), sqrt(d.cpr_pre), ylab="m.cpr_pre"))
legend("topleft", col=1:2, pch=1, legend=c("m.cpr_pre", "d.cpr_pre"))

# Create inverse variables. Mostly name them icp*, except use "roi"
# (return on investment) in place of icpr.
mobileAds2 <- mutate(mobileAds2,
                m.icpa_pre  = m.conv_pre / m.cost_pre,
                m.icpa_post = m.conv_post / m.cost_post,
                m.roi_pre   = m.value_pre / m.cost_pre,
                m.roi_post  = m.value_post / m.cost_post,
                d.icpa_pre  = d.conv_pre / d.cost_pre,
                d.icpa_post = d.conv_post / d.cost_post,
                d.roi_pre   = d.value_pre / d.cost_pre,
                d.roi_post  = d.value_post / d.cost_post)
# Plot ROI
with(mobileAds2,
     PlotSortGroups(m.roi_pre, m.roi_post, x3 = d.roi_pre, x4 = d.roi_post,
                    log="y", ylab = "roi"))
# That fails due to log(0).
# Can ignore warnings about: "log" is not a graphical parameter

# Plot log(1+roi) instead
par(mar=c(5.1, 4.1, 4.1, 4.1))
with(mobileAds2,
     PlotSortGroups(log1(cbind(m.roi_pre, m.roi_post, d.roi_pre, d.roi_post)),
                    ylab = "log(1 + roi), roi=value/cost"))
legend("topleft", col=1:4, pch=1,
       legend=c("m.roi_pre", "m.roi_post", "d.roi_pre", "d.roi_post"))
# 40% zeroes in m.roi_post, 20% zeroes in d.roi_post, about 15% in others
# ToDo: investigate that.
mtext(side=4, line=3, "roi = value/cost")
temp <- c(0, .25, .5, 1:4, 6, 10, 20, 40, 100)
axis(side=4, labels=temp, at=log1(temp))
par(mar=c(5.1, 4.1, 4.1, 2.1))

par(mfrow=c(1,3))
par(mar=c(4.2, 4.2, .3, .1), cex=.8, mex=.7)
with(mobileAds2,
     PlotSortGroups(m.cpr_pre, m.cpr_post, x3 = d.cpr_pre, x4 = d.cpr_post,
                    ylab = "cpr = cost/value (0 if value=0)", pch="."))
legend("topleft", col=1:4, pch=1,
       legend=c("m.cpr_pre", "m.cpr_post", "d.cpr_pre", "d.cpr_post"))
with(mobileAds2,
     PlotSortGroups(m.roi_pre, m.roi_post, x3 = d.roi_pre, x4 = d.roi_post,
                    ylab = "roi = value/cost", pch="."))
legend("topleft", col=1:4, pch=1,
       legend=c("m.roi_pre", "m.roi_post", "d.roi_pre", "d.roi_post"))
with(mobileAds2,
     PlotSortGroups(log1(cbind(m.roi_pre, m.roi_post, d.roi_pre, d.roi_post)),
                    ylab = "log(1 + roi), roi=value/cost", pch="."))
legend("topleft", col=1:4, pch=1,
       legend=c("m.roi_pre", "m.roi_post", "d.roi_pre", "d.roi_post"))


### Try to identify what relates to outliers.
# I suspect they are related to size.
with(mobileAds2, plot(sqrt(m.impr_pre), log1(m.roi_pre)))
with(mobileAds2, plot(sqrt(m.click_pre), log1(m.roi_pre)))
with(mobileAds2, plot(sqrt(m.conv_pre), log1(m.roi_pre)))
with(mobileAds2, plot(sqrt(m.impr_post), log1(m.roi_post)))
with(mobileAds2, plot(sqrt(m.click_post), log1(m.roi_post)))
with(mobileAds2, plot(sqrt(m.conv_post), log1(m.roi_post)))
with(mobileAds2, plot(sqrt(d.impr_pre), log1(d.roi_pre)))
with(mobileAds2, plot(sqrt(d.click_pre), log1(d.roi_pre)))
with(mobileAds2, plot(sqrt(d.conv_pre), log1(d.roi_pre)))
with(mobileAds2, plot(sqrt(d.impr_post), log1(d.roi_post)))
with(mobileAds2, plot(sqrt(d.click_post), log1(d.roi_post)))
with(mobileAds2, plot(sqrt(d.conv_post), log1(d.roi_post)))
# Yes, in all cases, more variability on the left

### Figure for paper, showing variability
par(mfrow=c(1,3))
par(mar=c(4.8, 4.8, .3, .1), cex=.6, mex=.6)
# ymax around 800 70 35
f <- function(n) ifelse(n==0, -.355, -1/(2+n^.2))
g <- function(x, ...) {
  plot(f(x), ..., axes=FALSE, xlab=deparse(substitute(x)));
  box(); axis(side=2)
}
axis1 <- function(x) axis(side=1, labels=x, at =f(x))
with(mobileAds2, g(d.impr_post, log1(d.roi_post)))
axis1(c(100, 200, 1000, 10^4, 10^5))
with(mobileAds2, g(d.click_post, log1(d.roi_post)))
axis1(c(5, 10, 20, 50, 100, 200, 500, 1000))
with(mobileAds2, g(d.conv_post, log1(d.roi_post)))
axis1(c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000))

# Want a single measure of size that can be used for all four groups.
# Take the minimum size across the groups (high variability is caused
# most by the smallest groups).
with(mobileAds2,
     plot(sqrt(pmin(m.impr_pre, m.impr_post, d.impr_pre, d.impr_post)),
          log1(m.roi_pre),
          xlab = "sqrt(fewest impressions among four groups)"))
with(mobileAds2,
     plot(sqrt(pmin(m.click_pre, m.click_post, d.click_pre, d.click_post)),
          log1(m.roi_pre),
          xlab = "sqrt(fewest clicks among four groups)"))
with(mobileAds2,
     plot(sqrt(pmin(m.conv_pre, m.conv_post, d.conv_pre, d.conv_post)),
          log1(m.roi_pre),
          xlab = "sqrt(fewest conversions among four groups)"))
# Again, more variability with smaller adgroups


# Compute errors on the scale of log(1 + roi)
# Don't take absolute values here.
mobileAds2 <-
  mutate(mobileAds2,
         error.log1.roi_pre = log1(m.roi_pre) - log1(d.roi_pre),
         error.log1.roi_post = log1(m.roi_post) - log1(d.roi_post))
# Check the distributions
qqnorm(mobileAds2$error.log1.roi_pre)
qqnorm(mobileAds2$error.log1.roi_post)
# Both are approximately normal, no big outliers

### Investigate relationship with mult.change
# mult.change is is the cumulative change from the experiment.
#Say the ad group started at 100% mobile bid multiplier, then clients
#makes the following changes during the experiment: (1) +20%, (2) -10%,
#and (3) +5%. Then, I sum them to total 115% (= 100% + 20% - 10% + 5%)
#with the mult.change columns reflected as 15%. Because advertisers
#don't implement changes exactly as recommended and are free to adjust
#throughout the experiment, this is a directional attempt to quantify
#the 'cause' to correlate with the 'effect'.
with(mobileAds, summary(mult.change)) # -1.64 to 1.62
with(mobileAds2, plot(mult.change, error.log1.roi_pre))
with(mobileAds2, cor(mult.change, error.log1.roi_pre))
# .47 - so multiplier tends to be higher when the roi for mobile was higher
# than the roi for desktop. Makes sense.
with(mobileAds2, plot(mult.change, error.log1.roi_post))
with(mobileAds2, cor(mult.change, error.log1.roi_post))
# .05 - much weaker correlation
with(mobileAds2, plot(mult.change, error.log1.roi_post - error.log1.roi_pre))
# negative correlation, as expected


prepost2 <- ggplot(data = mobileAds2) +
  geom_density(aes(x = error.log1.roi_pre, colour = "pre")) +
  geom_density(aes(x = error.log1.roi_post, colour = "post")) +
  xlab("Error in log(1 + roi)") +
  ylab("Distribution (Density)") + theme_bw() +
  ggtitle("Pre v Post") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.title = element_blank())
print(prepost2)
# nice plot, post looks clearly better
# (But is that just because there are more cases with roi=0, and these
# are easier to predict?)

# Can't combine normal and ggplot with mfrow, so use base graphics density
par(mfrow=c(1,2), mex=.8, cex=.8)
with(mobileAds2, plot(error.log1.roi_pre, mult.change))
with(mobileAds2,
     plot(density(error.log1.roi_post), lty=2, main="",
          xlab = "Error in log(1 + roi)"))
with(mobileAds2,
     lines(density(error.log1.roi_pre)))
abline(h=0, v=0, col="gray")
legend("topleft", lty=1:2, legend=c("pre", "post"))


mobileAds2 <- mutate(mobileAds2,
                abs.error.log1.roi_change =
                  abs(error.log1.roi_post) - abs(error.log1.roi_pre),
                sqr.error.log1.roi_change =
                  error.log1.roi_post^2 - error.log1.roi_pre^2)

prepost3 <- ggplot(data = mobileAds2) +
  geom_density(aes(x = abs.error.log1.roi_change)) +
  xlab("Change in absolute error in log(1 + roi)") +
  ylab("Distribution (Density)") + theme_bw() +
  ggtitle("Change in Error in log(1+roi)") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.title = element_blank())
print(prepost3)
# roughly symmetric

prepost4 <- ggplot(data = mobileAds2) +
  geom_density(aes(x = sqr.error.log1.roi_change)) +
  xlab("Change in squared error in log(1 + roi)") +
  ylab("Distribution (Density)") + theme_bw() +
  ggtitle("Change in Error in log(1+roi)") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.title = element_blank())
print(prepost4)
# much more peaked, but symmetric

# Look at other new ratio variable: icpa = conv/cost
par(mar=c(5.1, 4.1, 4.1, 4.1))
with(mobileAds2,
     PlotSortGroups(log1(cbind(m.icpa_pre, m.icpa_post, d.icpa_pre, d.icpa_post)),
                    ylab = "log(1 + icpa), icpa=conv/cost"))
legend("topleft", col=1:4, pch=1,
       legend=c("icpa_pre", "icpa_post", "d.icpa_pre", "d.icpa_post"))
#
mtext(side=4, line=3, "ipca = conv/cost")
temp <- c(0, .25, .5, 1:3)
axis(side=4, labels=temp, at=log1(temp))
par(mar=c(5.1, 4.1, 4.1, 2.1))
# More zeroes in the post period

# Investigate cases with roi=0; more of them in post period.
# Is it just due to smaller counts in post period?

with(mobileAds2, plot(sqrt(m.impr_pre), sqrt(m.impr_post), pch=".")); abline(0,1)
with(mobileAds2, plot(sqrt(m.click_pre), sqrt(m.click_post), pch=".")); abline(0,1)
with(mobileAds2, plot(sqrt(m.conv_pre), sqrt(m.conv_post), pch=".")); abline(0,1)
with(mobileAds2, plot(sqrt(d.impr_pre), sqrt(d.impr_post), pch=".")); abline(0,1)
with(mobileAds2, plot(sqrt(d.click_pre), sqrt(d.click_post), pch=".")); abline(0,1)
with(mobileAds2, plot(sqrt(d.conv_pre), sqrt(d.conv_post), pch=".")); abline(0,1)
# I'm not seeing big differences before and after

with(mobileAds2, Table(pos(m.impr_pre), pos(m.impr_post))) # all positive
with(mobileAds2, Table(pos(d.impr_pre), pos(d.impr_post))) # all positive
with(mobileAds2, Table(pos(m.conv_pre), pos(m.conv_post))) # more zeroes post
#           zero positive
#  zero        4       17
#  positive  141      428
with(mobileAds2, Table(pos(d.conv_pre), pos(d.conv_post))) # bit more zeroes post
#           zero positive
#  zero       26       36
#  positive   51      477
# We earlier omitted cases that didn't have clicks both pre and post.
# If include them the imbalance is more extreme

### Fewer clicks after
with(mobileAds, Table(pos(m.click_pre), pos(m.click_post)))
#           zero positive
#  zero        0        0
#  positive   47      608
with(mobileAds, Table(pos(d.click_pre), pos(d.click_post)))
#           zero positive
#  zero       10        1
#  positive   18      626

# Fewer conversions
with(mobileAds, Table(pos(m.conv_pre), pos(m.conv_post)))
#           zero positive
#  zero        5       17
#  positive  196      437
with(mobileAds, Table(pos(d.conv_pre), pos(d.conv_post)))
#           zero positive
#  zero       51       37
#  positive   63      504


# Fewer clicks afterward. Fewer conversions on the clicks.
colMeans(select(mobileAds, m.impr_pre, m.impr_post, d.impr_pre, d.impr_post))
# m.impr_pre m.impr_post  d.impr_pre d.impr_post
#   2625.602    2286.879   10904.380   12663.406
# after has fewer impressions on mobile, more on desktop
colMeans(select(mobileAds, m.click_pre, m.click_post, d.click_pre, d.click_post))
# m.click_pre m.click_post  d.click_pre d.click_post
#    75.23969     66.85191    125.51145    134.21221
# same as clicks
colMeans(select(mobileAds, m.conv_pre, m.conv_post, d.conv_pre, d.conv_post))
# m.conv_pre m.conv_post  d.conv_pre d.conv_post
#   13.97099    10.99084    27.61832    29.69466
# after has fewer conversions on mobile
colMeans(select(mobileAds2, m.conv_pre, m.conv_post, d.conv_pre, d.conv_post))
# m.conv_pre m.conv_post  d.conv_pre d.conv_post
#   14.89661    12.09661    29.43051    31.68305
# True to a lesser extent after removing data without clicks all four periods

zzz
# impr click cost conv value cpm=cost/impr cpc=cost/click cpa=cost/conv cpr=cost/value roi=value/cost

#--------------------------------------------------
# Tests

# Test statistic: T = sum(weight * abs.error.log1.roi_change)
# Permutation test: change sign on the errors
# Or normal approximation: under random sign changes on the errors,
#   E(T) = 0
#   var(T) = sum(weight^2 * error.log1.roi_change)

# Use clicks to weight (could use impressions or conversions)
weight <- with(mobileAds2, pmin(m.click_pre, m.click_post, d.click_pre, d.click_post))
testStatistic1 <- sum(weight * mobileAds2$abs.error.log1.roi_change)
testStatistic1
#[1] -649.3334
nullVar1 <- sum(weight^2 * mobileAds2$abs.error.log1.roi_change^2)
sqrt(nullVar1)
#[1] 510.2001
testStatistic1 / sqrt(nullVar1)
#[1] -1.272703

# That test statistic uses absolute errors - looking for smaller mean abs error.
# Squared errors are more common in statistics.
# That seems appropriate here, because the errors are approximatly normal,
# not long-tailed.
testStatistic2 <- sum(weight * mobileAds2$sqr.error.log1.roi_change)
testStatistic2
#[1] -261.339
nullVar2 <- sum(weight^2 * mobileAds2$sqr.error.log1.roi_change^2)
sqrt(nullVar2)
#[1] 495.7551
testStatistic2 / sqrt(nullVar2)
#[1] -0.5271535

# Neither test is discernible.


