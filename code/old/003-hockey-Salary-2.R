###############################################################
## Does the number of goals affect a hockey player's salary? #
###############################################################

# We are going to use a regression model to explain SALARY (annual, in dollars)
# by the number of points and some other variables.

# Step 1: Load the data
hockeyData <- read.csv("NHL1998-99.csv",header=TRUE,na.strings="-999")

# Step 2: Data selection
# I select only the players for which we have data
hockeyData <- subset(hockeyData,!is.na(hockeyData$SALARY))

# Set up the null: we want to show that more goals --> more money, c.p.
# So set H_0: beta_goals<=0
# Ha: beta_goals>0

# Step 3: Estimating a model
# Previous data exploration has suggested that the following
# is a reasonable model.
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
# Be careful with two-sidedness: see next week's exercises.
summary(model)
