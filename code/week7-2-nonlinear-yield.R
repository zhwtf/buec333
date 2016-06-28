####################################################
## Determining optimal temperature for yield
####################################################

# Details on data: see last section of "week7-1-hockey.R"

library(readr)
yield_df <- read_csv("yield.txt")

# Remember:
library(ggplot2)
qplot(Temperature,Yield,data=yield_df) + geom_smooth(se=FALSE)

# The plot suggests that the relationship
#   may be nonlinear.

# To fit a quadratic, first generate a new
#   explanatory variable
yield_df$temp2 <- yield_df$Temperature * yield_df$Temperature

# Then, use "lm" to fit a quadratic curve
yield_quad <- lm(Yield~Temperature+temp2,data=yield_df)
summary(yield_quad)
# The quadratic term is highly significant,
#   suggesting ...

# We can add this fit to the plot
qplot(Temperature,Yield,data=yield_df) + 
  geom_smooth(method="lm",formula=y~poly(x,2),se=FALSE)

# What is the optimal temperature?
b <- yield_quad$coefficients
tempstar <- - b[2] / (2*b[3])

# BONUS!
#
# Is tempstar a ... random variable?
#  Formula: - beta1_hat / (2*beta2_hat): yes   [1]
#  Number:  is a realization of the RV in [1]  [2]
#
