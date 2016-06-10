rm(list=ls())
#install.packages("foreign")
require(foreign)
CPS_data <- read.dta("cps92_08.dta")

# Compute average hourly earnings
ahe_average <- mean(CPS_data$ahe)

# Get the SE
se_Xbar <- sqrt(var(CPS_data$ahe) / nrow(CPS_data))

# Confidence intervals
c(Xbar-1.96*se_Xbar,Xbar+1.96*se_Xbar)

# as
CPS_data$ahe_2008 <- CPS_data$ahe / 140.3 * 215.2
ahe_2008_averbar <- mean(CPS_data$ahe_2008)

# Get the SE
se_Xbar <- sqrt(var(CPS_data$ahe_2008) / nrow(CPS_data))

# Confidence intervals
c(Xbar-1.96*se_Xbar,Xbar+1.96*se_Xbar)

x <- 5
x^2

x <- 7
x^2
