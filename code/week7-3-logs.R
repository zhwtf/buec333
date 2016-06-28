##################################
## Clear workspace, load packages
rm(list=ls())
require(foreign)
require(ggplot2)
library(stargazer)

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
model3 <- lm(lwage~educ+female+female*educ,data=wageData)
summary(model3)

stargazer(model1,model2,model3,type="text",keep.stat=c("n"))

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

#model4 <- lm(lwage~educ+female+ ???,data=wageData)

summary(lm(lwage~educ+female+female*educ,data=wageData))
summary(lm(lwage~educ+female+exper+married,data=wageData))
summary(lm(lwage~educ+female+exper+married+smsa+northcen+south+west,data=wageData))

## Q: The R^2 always increases. Is this a coincidence?
#
#
# Overall conclusion seems to hold steady.
# - c.p., a year of education gives you an expected higher wage of 8%
# - c.p., men and women have the same expected returns to education.
# - c.p., women expect to see a 30% lower wage.
######################################################

######################################################
## Alternative data set.
## This one is from 1980
## "Young men" in the National Longitudinal Survey (NLS)

wageData.2 <- read.dta("WAGE2.DTA")

# Data decription
# This is slightly more recent data, from 1980.
#
#   Obs:   935
# 1. wage                     monthly earnings
# 2. hours                    average weekly hours
# 3. IQ                       IQ score
# 4. KWW                      knowledge of world work score
# 5. educ                     years of education
# 6. exper                    years of work experience
# 7. tenure                   years with current employer
# 8. age                      age in years
# 9. married                  =1 if married
# 10. black                    =1 if black
# 11. south                    =1 if live in south
# 12. urban                    =1 if live in SMSA
# 13. sibs                     number of siblings
# 14. brthord                  birth order
# 15. meduc                    mother's education
# 16. feduc                    father's education
# 17. lwage                    natural log of wage

summary(wageData.2)

# "men": there is no gender dummy. So we cannot use this data
#        to test our conclusions about gender discrimination.

# "young". Substantially less work experience.
mean(wageData$exper)
mean(wageData.2$exper)

# First, note that wage is in "monthly earnings", so to compare,
# we first need to divide by the number of hours. Then, we take
# logs so that it is comparable to our previous study.
wageData.2$lwage <- log(wageData.2$wage/wageData.2$hours)

# Run the model, for men only. 
# Because the coefficient on educ did not vary with
#   gender, this should be comparable.
model1 <- lm(lwage~educ,data=wageData.2)
summary(model1)
# Q: An explanation for why it is much lower?


#####
#
# Q: What is the omitted variable in this regression?
#
#
####

# We have argued that there is an omitted variable bias
# in this relationship: "ability" may be driven by both.
# This data set measures IQ, which could be a proxy.
model2 <- lm(lwage~educ+IQ,data=wageData.2)
summary(model2)

stargazer(model1,model2,keep.stat=c("n"),type="text")
#
# Q: What do you conclude?

# What other variables are important?
# Well, we have another test result:
# KWW: "knowledge of the world." This may 
#     an interesting variable to control for,
#     if you are not convinced about IQ.
summary(lm(lwage~educ+IQ+KWW,data=wageData.2))

## Conclusion: it seems that we were overestimating
##    the return to education because of an omitted variable bias.

summary(lm(lwage~educ+IQ+KWW+age+tenure,data=wageData.2))
# Q: What is the interpretation of the coefficient estimates?

# An interesting question that this data set allows
#    you to answer is: 
# 
# Do first-borns earn more?
#   http://www.foxnews.com/health/2011/09/23/survey-birth-order-affects-job-salary/
summary(lm(lwage~educ+IQ+KWW+age+tenure+brthord,data=wageData.2))

####
#### BONUS
####
#### Actually, are first-borns smarter?
# Yes, says the news:
# http://www.nbcnews.com/id/38683279/ns/health-childrens_health/t/sorry-kid-first-borns-really-are-smarter/
iq_order <- lm(IQ~brthord,data=wageData.2)
summary(iq_order)
# Q: ?

#
#
#
#
#
# Q: Can you come up with an omitted variable?
#
#
#
#
#
#
#
#
#
#
# ? PARENT'S EDUCATION?
# - Perhaps higher educated parents have fewer/more children?
#   That makes their children more likely to be "first-born".
# - Education could be argued to be related with IQ,
#   and is possible hereditary.

# We could check that by
summary(lm(sibs~feduc+meduc,data=wageData.2))

# And then resolve this omitted variable problem by:
iq_order_ov <- lm(IQ~brthord+feduc+meduc,data=wageData.2)
summary(iq_order_ov)

stargazer(iq_order,iq_order_ov,type="text",keep.stat = c("n"))



