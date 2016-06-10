# Use software to load .dta
# install.packages("haven")
require(haven)
CPS_data <- read_dta("cps92_12.dta")

# ahe = average hourly earnings
# Average earnings for everybody in the sample
mean(CPS_data$ahe)

# Average for people in 2012
ahe_bar_92 <- mean(CPS_data$ahe[CPS_data$year==1992])
ahe_bar_12 <- mean(CPS_data$ahe[CPS_data$year==2012])

# Question 3.1 a ii
s_ahe_92 <- sd(CPS_data$ahe[CPS_data$year==1992])
s_ahe_12 <- sd(CPS_data$ahe[CPS_data$year==2012])

# Question 3.1 a iii
c(ahe_bar_92 - 1.96*s_ahe_92,ahe_bar_92 + 1.96*s_ahe_92)
c(ahe_bar_12 - 1.96*s_ahe_12,ahe_bar_12 + 1.96*s_ahe_12)

# Question 3.1 a iv
# 95%-CI for change in ahe from 1992 2012

ahe_diff <- ahe_bar_12 - ahe_bar_92

n_12 <- sum(CPS_data$year==2012)
n_92 <- sum(CPS_data$year==1992)

se_diff <- sqrt(s_ahe_12*s_ahe_12 / n_12 + s_ahe_92*s_ahe_92 / n_92)

c(ahe_diff - 1.96*se_diff,ahe_diff + 1.96*se_diff)
