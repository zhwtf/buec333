# Load the fatality data
library(haven)
beer_fatality <- read_dta("fatality.dta")
# summary(beer_fatality)

# Plot it
library("ggplot2")
qplot(beertax,mrall,data=beer_fatality)

# Conclusion: higher beertax, higher
#   fatality rate!

# Before regression, let's
#   scale up fatailities for readability
beer_fatality$mrall <- beer_fatality$mrall*100000

# In a regression:
ols_reg <- lm(mrall~beertax,data=beer_fatality)
summary(ols_reg)

# Plot for each year:
p <- qplot(beertax,mrall,data=beer_fatality)
p <- p + facet_wrap(~year)
p 

# Plot for each state
beer_fatality$id <- as.factor(beer_fatality$state)
p <- qplot(beertax,mrall,data=beer_fatality)
p <- p + facet_wrap(~id)
p 

# Conclusion: does not seem to be
#  increasing

# Fixed effects estimator
fe_reg <- lm(mrall~beertax+as.factor(state),data=beer_fatality)

library(stargazer)
stargazer(ols_reg,fe_reg,
          type = "text",
          keep = "beertax",
          keep.stat = c("n"))

