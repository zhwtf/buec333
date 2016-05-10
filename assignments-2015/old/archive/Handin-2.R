## Prelims
options(CRAN = "http://cran.stat.ucla.edu")
install.packages("foreign")

## Load the data
require(foreign)
unionData <- read.dta("http://www.sfu.ca/~cmuris/2014-Summer-333/wagepan.dta")
unionData <- na.omit(unionData)
str(unionData)

## Beforw e start: elicit tyime and home directory


## Q1: generate a wage variable.
unionData$wage <- exp(unionData$lwage)
summary(unionData$wage)

## Make the scatterplot
require(ggplot2)
qplot(union,wage,data=unionData)

## First regression
summary(lm(wage~union,data=unionData))

## Q. 4
summary(lm(lwage~union,data=unionData))

## Q. 6
summary(lm(lwage~union+hours+year+black+occ1+occ2+occ3+occ4+occ5+occ6+occ7+occ8+occ9+exper,data=unionData))
