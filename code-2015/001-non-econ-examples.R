########################
### DIAMONDS EXAMPLE ###
########################

# Load required package.
# For plotting, but it also contains the data sets 
rm(list=ls())
require(ggplot2)

# Q: How is a diamond's price
#    affected by its carat (mass)
data(diamonds)
# This data set is very large, so let's take
# a subset of 150 observations.
dsmall <- diamonds[sample(nrow(diamonds), 150), ]

# First, let's look at 
# 1. Plot carats and price
qplot(carat,price, data=dsmall)
# 2. Covariance. 
cov(dsmall$carat,dsmall$price)
# 3. Remember: covariance is hard to interpret...
# ... so we ask for the correlation, which is known to lie
# in between -1 and 1.
cor(dsmall$carat,dsmall$price)

# Do we now know what the sign of \hat{\beta} will be?
summary(lm(price~carat,data=dsmall))

# How to interpret this?
# A carat is 200 mg, or one-fifth of a gram. For every
# increase in the diamond's weight of 200 grams,
# we expect the diamond's price to go up by $ 8000,
# ceteris paribus.

# What about omitted variables?
# Are there other diamond characteristics, correlated
# with a diamond's size, that affect price?

# I do not know a lot about diamonds.
# Let's see what other variables are available:
names(dsmall)
# There are several candidates that influence
# the price (color, cut, clarity). Let's look at "cut".
# The cut is basically how the diamond is shaped during
#   polishing. It is important because it affects the
#   brilliance of the diamond.
#
# It is possible that the cut is related to the size,
#   because different types of diamond require different cuts.
#
# First, we must do this transformation to be able to work with cut
# Let's look at the "cut" variable.
summary(dsmall$cut)
# Use a dummy to group these into two categories 
# (You could do better than this)
dsmall$cutBinary <- ifelse(dsmall$cut %in% c("Premium","Ideal"),1,0)
# (manually check that this went well)

# Graphically check whether we can detect a correlation
# between cut and size, using a fancy version of the
# previous graph.
# One way to do this graphically is to make a more interesting graph,
p <- ggplot(aes(x=carat,y=price),data=dsmall)
p <- p + geom_point(aes(colour=as.factor(cutBinary)))
p
# One alternative would be to run a regression
summary(lm(carat~cutBinary,data=dsmall))
# Conclusion: does cutBinary "have an effect" on "carat"?

# Now, run the regression with the "omitted variable" cutBinary.
summary(lm(price~carat+cutBinary,data=dsmall))

# Remember the previous output
summary(lm(price~carat,data=dsmall))


##################
### CARS DATA ####
##################

rm(list=ls())

# Engine size and fuel economy for Audi's
data(mpg)
?mpg
# Single covariate regression:
# - hwy: highway miles per gallon ("fuel economy")
#   displ: engine displacement, in liters: engine size

# Smaller engines, lower economy?
qplot(displ, hwy, data=mpg) + xlab("engine size") + ylab("fuel economy")
cov(mpg$displ,mpg$hwy)
cor(mpg$displ,mpg$hwy)
summary(lm(hwy~displ,data=mpg))
# Interpret the coefficients...

# Homework: do an analysis similar to the one for diamonds.
# Two clues:

# 1. Number of cylinders
qplot(displ, hwy, data=mpg, colour=factor(cyl)) + xlab("engine size") + ylab("fuel economy") 

# 2. Manual or auto transmission
mpg$manual <- ifelse(mpg$trans %in% c("manual(m5)","manual(m6)"),1,0)
summary(lm(hwy~manual,data=mpg))