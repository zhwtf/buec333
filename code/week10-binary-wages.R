##################################
## Clear workspace, load packages
rm(list=ls())
require(foreign)
require(ggplot2)

## Load the data. We are interested in the
wageData <- read.dta("WAGE1.DTA")
wageData$female <- as.factor(wageData$female)
summary(wageData)
##################################

##################################
## Data from 1976, USA (Current POpulation Survey), 526 workers.
# 1. wage                     average hourly earnings
# 2. educ                     years of education
# 3. exper                    years potential experience
# 4. tenure                   years with current employer
# 5. nonwhite                 =1 if nonwhite
# 6. female                   =1 if female
# 7. married                  =1 if married
# 8. numdep                   number of dependents
# 9. smsa                     =1 if live in SMSA
# 10. northcen                 =1 if live in north central U.S
# 11. south                    =1 if live in southern region
# 12. west                     =1 if live in western region
# 13. construc                 =1 if work in construc. indus.
# 14. ndurman                  =1 if in nondur. manuf. indus.
# 15. trcommpu                 =1 if in trans, commun, pub ut
# 16. trade                    =1 if in wholesale or retail
# 17. services                 =1 if in services indus.
# 18. profserv                 =1 if in prof. serv. indus.
# 19. profocc                  =1 if in profess. occupation
# 20. clerocc                  =1 if in clerical occupation
# 21. servocc                  =1 if in service occupation
#
## Q: Which variables do you think are relevant here?
##################################

##################################
## A rough look at the data. #####
##################################
# Average wage level for women is much lower than for men
mean(wageData$wage[wageData$female==1]) #select only the women
mean(wageData$wage[wageData$female==0]) #select only the men

## Q: Is this gender discrimination?

# Average education levels are lower, too
mean(wageData$educ[wageData$female==1])
mean(wageData$educ[wageData$female==0])

## Q: What does this mean?

## Q: How do you interpret the following? 
mean(wageData$female==1,na.rm=TRUE)
## Q: Is it a population quantity?

# Let's graph it.
qplot(x=educ,y=lwage,col=female,data=wageData,xlab="Years of education",ylab="Log of hourly averages earnings")
# Q: Is this what you would expect if "gender" is an omitted variable?
# Q: What do you think about the role of gender in wages.
#############################

#############################
# REGRESSION APPROACH
#
# Let's use regression to answer this question.
#
# Step 1: Interpreting the coefficient in a semi-log model
model1 <- lm(lwage~educ,data=wageData)
summary(model1)
# Great, this agrees with our belief that higher levels of education
# lead to higher wages. Education is good!
#   (Note: people with higher ability (in error term) may choose
#      higher levels of education, so our result may be due to
#      omitted variable bias.)

# Interpretation of the coefficient of educ:
#   This requires an understanding of "logs", because we include
#      log of wages. We deal with this next week. For now, 
#      take it from me that:
#   increasing the years of education by 1 raises the hourly wage
#   by 8%, ceteris paribus.
#
# Q: Do you believe that our estimate beta-hat is an unbiased
#       estimate/estimator for this c.p. effect?

# Step 2: Given the education level, do men earn more than women?
model2 <- lm(lwage~educ+female,data=wageData)
summary(model2)
#
# First: should we include gender dummy in the model?
#        1. Common sense/theory/background knowledge: yes
#        2. t-test: yes
#        3. Affects other variables: yes, returns to education estimate changes
#        4. R-squared rises steeply.
#
# Interpreting the results:
#        EDUC: In this model, we estimate that taking one additional year of
#              schooling increases hourly wage by 7.7 percent. This is
#              lower than before.
#        FEMALE: Ceteris paribus, women earn 36% less

# Step 3: Are the returns to education different for men and for women?
# Pictures first
qplot(x=educ,y=lwage,col=female,data=wageData,xlab="Years of education",ylab="Log of hourly averages earnings") + 
  geom_smooth(method=lm)

# Q: Do you think that a model with educ*female is useful?
# Q: Which regression would you run to test your answer?
model3 <- lm(lwage~educ+female+female*educ,data=wageData)
summary(model3)
#
# Should we include this interaction term?
#
#        Not significant, does not change other coefficients, R2 drops.

# Step 4: Perhaps there are other, omitted variables that we are not controlling for?
summary(wageData)
#  1. wage                     average hourly earnings
#  2. educ                     years of education
#  3. exper                    years potential experience
#  4. tenure                   years with current employer
#  5. nonwhite                 =1 if nonwhite
#  6. female                   =1 if female
#  7. married                  =1 if married
#  8. numdep                   number of dependents
#  9. smsa                     =1 if live in SMSA
# 10. northcen                 =1 if live in north central U.S
# 11. south                    =1 if live in southern region
# 12. west                     =1 if live in western region
# 13. construc                 =1 if work in construc. indus.
# 14. ndurman                  =1 if in nondur. manuf. indus.
# 15. trcommpu                 =1 if in trans, commun, pub ut
# 16. trade                    =1 if in wholesale or retail
# 17. services                 =1 if in services indus.
# 18. profserv                 =1 if in prof. serv. indus.
# 19. profocc                  =1 if in profess. occupation
# 20. clerocc                  =1 if in clerical occupation
# 21. servocc                  =1 if in service occupation
# 22. lwage                    log(wage)
# 23. expersq                  exper^2
# 24. tenursq                  tenure^2

## model4 <- lm(lwage~educ+female+ ???,data=wageData)

summary(lm(lwage~educ+female+female*educ,data=wageData))
summary(lm(lwage~educ+female+exper+married,data=wageData))
summary(lm(lwage~educ+female+exper+married+smsa+northcen+south+west,data=wageData))

## Q: The R^2 always increases. Is this a coincidence?
#
# Overall conclusion seems to hold steady.
# - c.p., a year of education gives you an expected higher wage of ~8%
# - c.p., men and women have the same expected returns to education.
# - c.p., women expect to see a 30% lower wage.
######################################################