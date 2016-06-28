####################################################
## Why do some hockey players earn more than others?
####################################################

#
# We are going to use a regression model to explain
#
# SALARY (annual, in dollars)
#
# by
#
# ??
#
# for a population of hockey players in the NHL,
# in 1998-1999.


# Exploratory analysis ----------------------------------------------------

# Step 1: Load the data
hockeyData <- read.csv("NHL1998-99.csv",header=TRUE,na.strings="-999")

# Step 2: Exploratory data analysis:
# - What do the variables mean?
# - Anything funny going on?
# - 
summary(hockeyData)

# Issue 1: Two Petr Sykora's!
# print all the names, then extract the info for them
hockeyData$PLAYER
# This identifies them as players 708 and 709
hockeyData[c(708,709),]

# Let's look at some data
require(ggplot2)
qplot(SALARY,data=hockeyData)

## Who is the big earner?
#Find the maximum salary
bigsalary <-  max(hockeyData$SALARY,na.rm=TRUE)
bigsalary
#Find the row in the data that has this player
index <- which(hockeyData$SALARY == bigsalary)
index
#Print player info
hockeyData[index,]

## And the small one?
smallsalary <-  min(hockeyData$SALARY,na.rm=TRUE)
index2 <- which(hockeyData$SALARY == smallsalary)
#Print player info
hockeyData[c(index,index2),]

# This makes me think that ... matters

# Issue 2: Missing data
summary(hockeyData)
# The data on 142 players is missing.
# Is this a problem?

# I select only the players for which we have data
hockeyData <- subset(hockeyData,!is.na(hockeyData$SALARY))

# Finally: express salary in $1000's of dollars.
hockeyData$SALARY <- hockeyData$SALARY/1000

# Multicollinearity -----------------------------------

# Issue 3: The simplest model.
# points = goals + assists
# Say that you believe that this determines how valuable a player is
model1 <- lm(SALARY~Points,data=hockeyData)
summary(model1)

# Successful model! I want to refine it and include goals and assists as well
# so that I know where the value comes from: scoring or helping to score
model2 <- lm(SALARY~Points+Goals+Assists,data=hockeyData)
summary(model2)

# WHat is going on?
# Note R^2 did not really go up. Let's stick with "points" only.


# Omitted variables (1) -------------------------------------------------------

# Do you think that we can improve on this model?
# I am worried about omitted variables. How about including a dummy for
# somebodies position?
model3 <- lm(SALARY~Points+Position,data=hockeyData)
summary(model3)

# Show the regression output side-by-side
library(stargazer)
stargazer(model1,model3,
          title="Results: OV bias",type="text")


# Frisch-Waugh-Lovell -----------------------------------------------------

# FWL: Running a regression of Y on X1 and other Xs 
#      is the same as:
#
# 1. Run a regression of X1 on X2, residuals X1tilde
# 2. Run a regression of Y on X2, residuals Ytilde
# 3. Run a regression of Ytilde on X1tilde
Ytilde <- residuals(lm(SALARY~Position,data=hockeyData))
X1tilde <- residuals(lm(Points~Position,data=hockeyData))
summary(lm(Ytilde~X1tilde))
# Same as output from Model 3!


# Omitted variables (2) ---------------------------------------------------

# What other variables would you include?
model4 <- lm(SALARY~Points+Position+AGE+Years_EXP,data=hockeyData)
summary(model4)

# What about some unlikely variables: LvR-handedness
model5 <- lm(SALARY~Points+Position+AGE+Years_EXP+Handed,data=hockeyData)

stargazer(model1,model3,model4,model5,
          title="OV",keep.stat = c("n"),type="text")


# Hypothesis testing ------------------------------------------------------

# Does scoring more goals increase your salary?
# Say that we like the following model.
model <- lm(SALARY~Goals+Assists+Position+Handed+AGE,data=hockeyData)
summary(model)

# Testing, 1:
# calculation by hand
tval <- 24762/7462
tval

# Test 2: Any values of null hypothesis in the confidence interval?

alpha <- 0.05
lev <- 1-alpha
confint(model, parm = "Goals",level=lev)

# Test 3: Just look at the p-value or t-value! See book, page 135, 136
summary(model)


# Nonlinear relationships -------------------------------------------------

# From 
#  https://onlinecourses.science.psu.edu/stat501/node/325
#
library(readr)
yield_df <- read_csv("yield.txt")
# An experiment that examines the yield for a certain crop,
#  as a function of the (set) temperature of its environment.
qplot(Temperature,Yield,data=yield_df)
qplot(Temperature,Yield,data=yield_df) + geom_smooth(se=FALSE)

# Does this look linear?

# What happens when you use the following output
#   to decide whether to increase the temperature?
qplot(Temperature,Yield,data=yield_df) + 
  geom_smooth(se=FALSE) +
  geom_smooth(method="lm",se=FALSE,aes(color="red"))

# You find that the effect is negative:
#   higher temperatures are bad!
#
# You decide to set the temperature to 50.
#
# However, once you see the nonlinear curve,
#   you will correctly set temperature to ~70.

# Regression output
summary(lm(Yield~Temperature,data=yield_df))
# You incorrectly conclude that there is
#   no effect from temperature!

# Problem: Assumption 1 is violated
#   for the linear regression line!
#
# How can you see that?

