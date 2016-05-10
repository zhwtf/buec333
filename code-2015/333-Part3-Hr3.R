####################################################
## WHy do some hockey players earn more than others?
####################################################

# We are going to use a regression model to explain
#
# SALARY (annual, in dollars)
#
# by
#
# ??

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

# Do you think that we can improve on this model?
# I am worried about omitted variables. How about including a dummy for
# somebodies position?
model3 <- lm(SALARY~Points+Position,data=hockeyData)
summary(model3)

# What other variables would you include?
model4 <- lm(SALARY~Points+Position+AGE+Years_EXP,data=hockeyData)
summary(model4)

# What about some unlikely variables:

# what do you expect for left- versus right-handedness: does it matter?
model4 <- lm(SALARY~Points+Position+AGE+Years_EXP,data=hockeyData)

#### HYPOTHESIS TESTING
# Does scoring more goals increase your salary?
# Say that we likethe following model.
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

