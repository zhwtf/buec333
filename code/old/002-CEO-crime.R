## Examples inspired by
## Wooldridge (2009), Introductory Econometrics, 4e
##

### Example 1: CEO data

# Data on 209 CEO's in 1990, obtained from Business Week

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

summary(lm(salary ~ profits, data=CEOdata ))

# Question: should we also include tenure, i.e.
#  how many years have you been with this company?

# 1: Common sense: yes!
summary(lm(salary ~ profits + comten, data=CEOdata ))
# 2. Adjusted R-squared: down
# 3. It seems to have the wrong sign.
# 4. Does not seem to matter for the coefficient estimate for "profits"
# --> comten does not seem to help.

# Perhaps we were including the wrong variable. How about ceoten?
summary(lm(salary ~ profits + ceoten, data=CEOdata ))

# Conclusion?

# We should have inspected the data!
require(ggplot2)
qplot(salary,data=CEOdata) 
# There is an outlier! This could be a violation of assumptions 2/3.

qplot(ceoten,salary,data=CEOdata)
qplot(ceoten,lsalary,data=CEOdata)

# It is more natural to look at logs (see next week)
summary(lm(salary ~ profits + ceoten, data=CEOdata ))
summary(lm(lsalary ~ profits + ceoten, data=CEOdata ))

# Warning: You cannot compare the R^2 across
# these models, because they have different Y's!

# What other crucial variables are missing?
summary(lm(lsalary ~ profits + ceoten + lsales , data=CEOdata ))
# How do you interpret this?
# Because both salary and sales are in logs, we will see next week that
# the coefficient on (log) sales is an elasticity:
# a 1% increase in sales will drive up salary by 0.2%

# What about market value?
summary(lm(lsalary ~ profits + ceoten + lsales + mktval , data=CEOdata ))

# It seems that (log) sales is a strong variable here, in terms of t-statistic
# Does that make sense?

# So what should you do as a CEO?

# Visually, the correlation seems to be pretty strong.
qplot(lsales,lsalary,data=CEOdata)



##########################################################3
## Example 3: Crime
##########################################################3

# Sample of males

crimeData <- read.table("CRIME1.raw",col.names=c("narr86","nfarr86","nparr86","pcnv","avgsen","tottime","ptime86","qemp86","inc86","durat","black","hispan","born60","pcnvsq","pt86sq","inc86sq"))
summary(crimeData)

#  1. narr86                   # times arrested, 1986
#  2. nfarr86                  # felony arrests, 1986
#  3. nparr86                  # property crme arr., 1986
#  4. pcnv                     proportion of prior convictions
#  5. avgsen                   avg sentence length, mos.
#  6. tottime                  time in prison since 18 (mos.)
#  7. ptime86                  mos. in prison during 1986
#  8. qemp86                   # quarters employed, 1986
#  9. inc86                    legal income, 1986, $100s
# 10. durat                    recent unemp duration
# 11. black                    =1 if black
# 12. hispan                   =1 if Hispanic
# 13. born60                   =1 if born in 1960
# 14. pcnvsq                   pcnv^2
# 15. pt86sq                   ptime86^2
# 16. inc86sq                  inc86^2

# Homework!



