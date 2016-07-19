#' More on the returns to the education.
#' Alternative data set.
#' This one is from 1980
#' "Young men" in the National Longitudinal Survey (NLS)

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

# We have argued that there is an omitted variable bias
# in this relationship: "ability" may be driven by both.
# This data set measures IQ, which could be a proxy.
model2 <- lm(lwage~educ+IQ,data=wageData.2)
summary(model2)
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
#

#' ##################
#' BONUS question! ##
#' ##################

#' An interesting question that this data set allows
#'    you to answer is: 
#' 
#' "Do first-borns earn more?"
#' 
#' "Yes", says Fox News.
#  http://www.foxnews.com/health/2011/09/23/survey-birth-order-affects-job-salary/
summary(lm(lwage~educ+age+tenure+brthord,data=wageData.2))
#'
#' Perhaps they earn more because they are smarter?
#' 
#' Makes sense, says NBC news:
# http://www.nbcnews.com/id/38683279/ns/health-childrens_health/t/sorry-kid-first-borns-really-are-smarter/
summary(lm(IQ~brthord,data=wageData.2))
#' 
#' Q: ?
#' "If my older brother had been my younger brother, I would have been
#'     substantially smarter?"
#'     
#' Q: Can you come up with an omitted variable?
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
# ? PARENT'S EDUCATION?
# - Perhaps higher educated parents have fewer/more children?
#   That makes their children more likely to be "first-born".
# - Education could be argued to be related with IQ,
#   and is possible hereditary.

#' We could check that by
summary(lm(sibs~feduc+meduc,data=wageData.2))

#' And then resolve this omitted variable problem by:
summary(lm(IQ~brthord+sibs+feduc+meduc+feduc:meduc,data=wageData.2))

#' Back to our original question: do first-borns earn more?
summary(lm(lwage~educ+age+tenure+brthord,data=wageData.2))
#' However, first-borns were found to be smarter (correlation!)
summary(lm(lwage~educ+IQ+KWW+age+tenure+brthord,data=wageData.2))
#' Correlation disappears: firstborns are paid more,
#'   but that finding disappears once we take into account
#'   their higher level of productivity!
#'
#' Also note: returns to education have dropped to ~3.5%!