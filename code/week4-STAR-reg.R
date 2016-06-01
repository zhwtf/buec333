######################
#
# We will use data on primary schools
#   in California to study the difference
#   in student performance in small
#   versus large class rooms.
#
# This version adds regression,
#   is used for demonstrating R Markdown.
#
######################

# A "small classroom" is defined as
#   the number of students per teacher
#   lower than "cutoff".
cutoff <- 20

# Load the project STAR data
require(readr)
school_df <- read_csv("testscores_california_1999.csv")

# What does the data look like?
summary(school_df)

# The number of students per teacher
#   is the class size
require(dplyr)
school_df <- mutate(school_df, class_size = enrl_tot / teachers)

# Look at the distribution of class size
require(ggplot2)
qplot(data=school_df,class_size)

# Manipulate
small_df <- filter(school_df,class_size<cutoff)
large_df <- filter(school_df,class_size>=cutoff)

# Sample mean, variance
#    for the schools with small classes
X_bar_small <- mean(small_df$testscr)
s2_small <- var(small_df$testscr)
n_small <- nrow(small_df)

# A 95% confidence interval for mu
X_bar_small + sqrt(s2_small/n_small)*c(-1.96,1.96)

# For the large classes
X_bar_large <- mean(large_df$testscr)
s2_large <- var(large_df$testscr)
n_large <- nrow(large_df)
X_bar_large + sqrt(s2_large/n_large)*c(-1.96,1.96)

## Q: Conclusion?

# Now, we can construct the confidence interval
#   for the difference.
DX_bar <- X_bar_small - X_bar_large
DX_bar
Ds2 <- s2_small / n_small + s2_large / n_large
Ds2
# Use (3.19)
DX_bar + sqrt(Ds2) * c(-1.96,1.96)
DX_bar + sqrt(Ds2) * 1.96


# p-value for H0: mu-small - mu-large = 0
tact <- DX_bar / sqrt(Ds2) # t-value
2*pnorm(-abs(tact)) #pnorm ~ Table 1 on page 757.

# However, consider

vars <- c("comp_stu", "expn_stu", "avginc", "el_pct")
cbind(colMeans(small_df[,vars]),
      colMeans(large_df[,vars]))


######################################
# Now, repeat for "cutoff <- 16".
# What changes?
##
#
# Next: regression
#
######################################

# Using the same data set, let's plot the
#   relationship between test scores and
#   class size.

plot <- ggplot(data=school_df,aes(x=class_size,y=testscr)) +
           geom_point() + 
           geom_smooth(method="lm",se=FALSE)

plot
ggsave("nice_plot.pdf",width=10,height=10)

# The plot above includes the sample regression line
# Let's look at the numerical regression results

# To compute the OLS estimates,
#   i.e. to "run a regression",
#   use the function lm
?lm

ols_classsize <- lm(testscr ~ class_size,data=school_df)
#                     Y         X
summary(ols_classsize)

# Based on today's lecture,
#    we can interpret only the first column
#
# Q: What is the difference in prediction
#      for two schools whose class-size differs by one?
#
# A: School with larger class-size has test-scores
#      lower by 2.3.
#
# JUMPING AHEAD BY ONE WEEK
# What's with the standard errors etc?
# -  The OLS estimator is a random variable!
# -  The reported p-value is for H0: beta_1 = 0
# -  Do we have sufficient evidence to conclude that
#       class-size are related with test scores?
#
# 
# JUMPING AHEAD TO CHAPTER 6
# Remember that other variables were also 
#    different across schools with different class-sizes
cbind(colMeans(small_df[,vars]),
      colMeans(large_df[,vars]))

# Perhaps these variables also influence test scores
#   -> Multiple linear regression
mlr <- lm(testscr ~ class_size + avginc, data=school_df)
summary(mlr)

# How about the percentage of English learners?
mlr2 <- lm(testscr ~ class_size + avginc + el_pct, data=school_df)
summary(mlr2)

# What has happened with the correlation between
#   class size and test scores?

###############################
## NEXT, TAKE THIS TO MARKDOWN
##
###############################