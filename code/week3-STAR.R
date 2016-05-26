######################
#
# We will use data on primary schools
#   in California to study the difference
#   in student performance in small
#   versus large class rooms.
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
######################################