## Examples inspired by
## Wooldridge (2009), Introductory Econometrics, 4e
##
rm(list=ls())
require(ggplot2)

############################
### Example 1: CEO data  ###
############################

# Data on 209 CEO's in 1990, obtained from Business Week Magazine
# What determines a CEO's salary?

CEOdata <- read.table("CEOSAL2.raw",col.names=c("salary","age","college","grad","comten","ceoten","sales","profits","mktval","lsalary","lsales","lmktval","comtensq","ceotensq","profmarg"))
summary(CEOdata)

#  1. salary                   1990 compensation, $1000s
#  2. age                      in years
#  3. college                  =1 if attended college
#  4. grad                     =1 if attended graduate school
#  5. comten                   years with company
#  6. ceoten                   years as ceo with company
#  7. sales                    1990 firm sales, millions
#  8. profits                  1990 profits, millions
#  9. mktval                   market value, end 1990, mills.
# 10. lsalary                  log(salary)
# 11. lsales                   log(sales)
# 12. lmktval                  log(mktval)
# 13. comtensq                 comten^2
# 14. ceotensq                 ceoten^2
# 15. profmarg                 profits as % of sales

# Starting model:
# Linear / linear: does a CEO's salary depend on profits?
summary(lm(salary ~ profits, data=CEOdata ))

## Log / log
# Why would we use this model?
qplot(salary,data=CEOdata)
qplot(lsalary,data=CEOdata)

# Now, create the log profits variable
CEOdata$lprofits <- log(CEOdata$profits)
# Q: What happened here?
# Take care of the problem.
CEOdata <- subset(CEOdata,profits>0)
CEOdata$lprofits <- log(CEOdata$profits)
# Run the log-log regression
summary(lm(lsalary ~ lprofits, data=CEOdata ))

# For comparison, here is the linear regression
summary(lm(salary ~ profits, data=CEOdata ))
# Q: Which one do you prefer?
#
# !: Warning: You cannot compare the R^2 between this
#    and the previous model!

# What is the role of other variables?
# Consider the following reasonable ones. (Right?)
# Note that we include "sales" because of a common discussion in business.
summary(lm(lsalary ~ lprofits + lsales + ceoten + age + mktval, data=CEOdata ))
# Q: What is your conclusion?

# I will omit profits and age, as I believe that they were only trying...
#   ... to capture what lsales and ceoten are really capturing.
summary(lm(lsalary ~ lsales + ceoten + mktval, data=CEOdata ))

## Log-linear specification.
summary(lm(lsalary ~ sales + ceoten + mktval, data=CEOdata ))

# Q: What is the interpretation for beta?
# Q: Which model do you prefer, log-log or log-linear? Why?

# Visually, the correlation seems to be pretty strong.
qplot(lsales,lsalary,data=CEOdata)

