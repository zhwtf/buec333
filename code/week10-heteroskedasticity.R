
# Introduction --------------------------------------------------
#
# We will use a data set
#   that accompanies Wooldridge's textbook.
#
# The data set and description
#   can be found here
#   https://ideas.repec.org/p/boc/bocins/fertil2.html
#
# The analysis here follows that from
#   an r-bloggers.com contribution, here:
#   http://www.r-bloggers.com/standard-robust-and-clustered-standard-errors-computed-in-r/
#

# Loading data ------------------------------------------------------------

# The data is in dta format.
# I will use the haven package, and use "read_dta",
#  For more information, see: https://github.com/hadley/haven
library(haven)
fertility_data <- read_dta("fertil2.dta")

# Inspect the data
summary(fertility_data)
#fertility_data
#fertility_data <- subset(fertility_data,urban==1)
fertility_data <- subset(fertility_data,agefbrth<35)
fertility_data <- subset(fertility_data,idlnchld>0)

# Codebook ----------------------------------------------------------------

# Obs:  4361

# 1. mnthborn                 month woman born
# 2. yearborn                 year woman born
# 3. age                      age in years
# 4. electric                 =1 if has electricity
# 5. radio                    =1 if has radio
# 6. tv                       =1 if has tv
# 7. bicycle                  =1 if has bicycle
# 8. educ                     years of education
# 9. ceb                      children ever born
# 10. agefbrth                 age at first birth
# 11. children                 number of living children
# 12. knowmeth                 =1 if know about birth control
# 13. usemeth                  =1 if ever use birth control
# 14. monthfm                  month of first marriage
# 15. yearfm                   year of first marriage
# 16. agefm                    age at first marriage
# 17. idlnchld                 'ideal' number of children
# 18. heduc                    husband's years of education
# 19. agesq                    age^2
# 20. urban                    =1 if live in urban area
# 21. urbeduc                  urban*educ
# 22. spirit                   =1 if religion == spirit
# 23. protest                  =1 if religion == protestant
# 24. catholic                 =1 if religion == catholic
# 25. frsthalf                 =1 if mnthborn <= 6
# 26. educ0                    =1 if educ == 0
# 27. evermarr                 =1 if ever married

# Estimation --------------------------------------------------------------

# We would like to model the fertility decision:
#  what influences a woman's choice to have a baby.
#
# A simple economic model would take into account:
# - preferences: how many children are desired
# - technology: access to (info about) anticonception
# - cost: caring for a child is more expensive
#         (daycare, opportunity cost) at higher levels of educ
#         since wages are generally higher
# - uncertainty: high-age birth are risky, but older women
#                    will have had more time to have children
#                religion can be examples of 
#                safety nets that make it less risky to raise a child
# 
reg_hmsk <- lm(ceb~idlnchld+
                     knowmeth+usemeth+electric+radio+tv+
                     educ+heduc+urbeduc+
                     age+agesq+
                     spirit+protest+catholic,
                     data=fertility_data)

summary(reg_hmsk)

# All variables come up significant!

# Heteroskedastic SE ------------------------------------------------------

# Use the information on implementing
#   robust standard errors here:
#   http://drewdimmery.com/robust-ses-in-r/

# Load the packages needed to implement
#   robust standard errors.
require("sandwich")
require("lmtest")
reg_htrsk <- reg_hmsk
reg_htrsk$newse<-vcovHC(reg_htrsk,type="HC1")
results_htrsk <- coeftest(reg_htrsk,reg_htrsk$newse)
results_htrsk

# Comparison --------------------------------------------------------------

# Extract and compare
results_hmsk <- summary(reg_hmsk)$coefficients

print(cbind(results_hmsk[,3],results_htrsk[,3]),
      digits=2)

# Interpretation:
#
# Find the variable for which a t-test
#   would now reject...
#
# It is urbeduc.
#
# What does that tell you, once you allow
#   for heteroskedasticity?
