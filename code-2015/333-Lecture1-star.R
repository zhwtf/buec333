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
ggsave("333-Lecture01-scatter.pdf")

#Fitting a straight line
qplot(str,testscr,data=csreduced,geom=c("point","smooth"),method="lm")
ggsave("333-Lecture01-fitted.pdf")

#Getting the numbers.
#Fit a line (using methods you will learn this semester)
starLM <- lm(testscr~str,data=csreduced)
#Display the results
summary(starLM)

# Conclusion:
# Using this model, we find that:
# 1. increasing the student-teacher-ratio...
#    ceteris paribus, reduces the performance in terms of
#    the average 5th grade test score.
# 2. Our best estimate is that, if str increases by one,
#    we expect performance to fall by -2.2.
#    Remember that test scores were between 600 and 700.
#
# Remember:
# 3. We have no idea where these numbers come from
# 4. We did not involve any other variables
# 5. We have assumed linearity (and a lot more!)
