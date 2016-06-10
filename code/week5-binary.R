#######################################
## BINARY VARIABLES: WAGE AND GENDER ##
#######################################

## Clear workspace, load packages
rm(list=ls())
require(foreign)
require(ggplot2)

## Load and inspect the data, 
wageData <- read.dta("WAGE1.DTA")
wageData$female <- as.factor(wageData$female)
#summary(wageData)
mean(wageData$wage)

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
##########################

## The wage gap
##
# Average wage level for women is much lower than for men
Ybar_female <- mean(wageData$wage[wageData$female==1]) #select only the women
Ybar_male <- mean(wageData$wage[wageData$female==0]) #select only the men
wage_gap <- Ybar_female - Ybar_male
wage_gap

# This wage gap is equal to the coefficient
#   on "female" in the following regression:
wage_regression <- lm(wage~female,data=wageData)
summary(wage_regression)

## Testing: statistically significant wage gap?
##
# Method 1: p-value for E(wage_male) = E(wage_female)
DX <-  wage_gap
s2female <- var(wageData$wage[wageData$female==1])
s2male <- var(wageData$wage[wageData$female==0])
nfemale <- sum(wageData$female==1)
nmale <- sum(wageData$female==0)
DX_SE <-  sqrt(s2female / nfemale + s2male / nmale)
tact <- DX / DX_SE
2*pnorm(-abs(tact))

# Method 2: read off the p-value for H0: beta1=0
summary(wage_regression)

## Difference: rounding errors.

## Q: Is this gender discrimination?
## Q: What else is in the error term?

# Average education levels are lower for women 
# (Remember: US in 1976)
educ_female <- mean(wageData$educ[wageData$female==1])
educ_male <- mean(wageData$educ[wageData$female==0])
educ_gap <- educ_female - educ_male
educ_gap

# Does this matter for the wage gap?
qplot(x=educ,y=lwage,col=female,data=wageData,xlab="Years of education",ylab="Log of hourly averages earnings")

## Another way to use indicator variables
##   is to run separately a regression for men
##   and women
summary(lm(wage~educ,data=wageData,subset = (female==1)))  #1
summary(lm(wage~educ,data=wageData,subset = (female==0)))  #2

# This gives the same information as 
#      one regression with all the data
summary(lm(wage ~ female + educ + female*educ,data=wageData)) #3

# How do you reconcile the output from #3 with #1 and #2

# Q: What do you think about the role of gender in 
#    wages in the United States in 1976?
