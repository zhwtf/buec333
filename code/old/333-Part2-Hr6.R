#Read the data file and save the results
csdata <- read.csv("testscores_california_1999.csv")

#Have a quick look at the data
#Just to check that the data import went well
#First 6 lines
head(csdata)

#Last 6 lines
tail(csdata)

#Display a summary of the data
summary(csdata)

#We are interested in the relationship between
# testscr: the average reading/math score in grade 5
# str: student to teach ratio
#Delete all the other variables, as the previous summary
#was messy
csreduced <- subset(csdata,select=c("testscr","str"))

#Now ask for the summary of the reduced data
summary(csreduced)

#For plotting, we use "ggplot2"
#This command tells R to load this additional
#piece of software ("package")
require(ggplot2)

#Visualize the test score data
qplot(testscr,data=csreduced)
ggsave("333-Lecture01-testscr.pdf")

#Visualize the student-to-teacher ratio data
qplot(testscr,data=csreduced)
ggsave("333-Lecture01-str.pdf")

#Scatterplot
qplot(str,testscr,data=csreduced)
ggsave("str-testscr-scatter.pdf")

#Fitting a straight line
qplot(str,testscr,data=csreduced,geom=c("point","smooth"),method="lm",se=FALSE)
ggsave("str-testscr-fitted.pdf")

#Getting the numbers.
#Fit the LS regression line
starLM <- lm(testscr~str,data=csreduced)
#Display the results
sum.results <- summary(starLM)
sum.results

#Print the R-squared
sum.results$r.squared
# Conclusion: ~5% of student's test scores can be explained
#   by the number of students in their class!

# Confidence interval
# Let's look at the structure of sum.results
str(sum.results)
# We need the "coefficients" part!
print(sum.results$coefficients)
b1.hat <- sum.results$coefficients[2,1]
b1.hat
sb1.hat <- sum.results$coefficients[2,2]
sb1.hat
b1.CI <- b1.hat + sb1.hat * c(-1.96,1.96) # can improve on this!
print(b1.CI)

# Testing
# look at output
summary(starLM)
# consider t-value and p-value in R's output


# Conclusion:
# Using this model, we find that:
# Option 1: Assumption 1 holds:
#    THEN: increasing the student-teacher-ratio...
#    ceteris paribus, reduces the performance in terms of
#    the average 5th grade test score. 
#
#    if str increases by one,
#    we expect performance to fall by -2.2, c.p..
#
#    Remember that test scores were between 600 and 700.
# 
# Option 2: Assumption 2 does not hold:
#    THEN?
#
# Remember:
# - We did not involve any other variables
# - We have assumed linearity (and a lot more!)
