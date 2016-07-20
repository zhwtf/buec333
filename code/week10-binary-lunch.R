
# Introduction ------------------------------------------------------------

# An important use for dummy variables is in
#  "program evaluation". In program evaluation
#  we study the response of individuals' behavior
#  to a (government) policy.
#
# If we can use econometrics to uncover the causal
#  effect of the policy, then we can do a cost-benefit
#  analysis:
#
# COST: from administrative records: how expensive was
#       it to implement the program
# BENEFITS: can be estimated using beta-hat.
#
# Below, we look at a program that gives free lunch
#  to primary school students from low-SES environments.
#
# The idea is that, by providing them with access to
#  free, healthy, food, their school performance will
#  increase.
#
# Q1. Possible channels?
#
# Q2. Keep an eye out for one of the threats
#       to internal validity! (Chapter 9)
#       Which of the threats should we be worried about?

# Loading data ------------------------------------------------------------

lunchData <- read.table("MEAP93.raw",col.names=c("lnchprg","enroll","staff","expend","salary","benefits","droprate","gradrate","math10","sci11","totcomp","ltotcomp","lexpend","lenroll","lstaff","bensal","lsalary"))
summary(lunchData)

#  1. lnchprg                  perc. of studs. in sch. lunch prog.
#  2. enroll                   school enrollment
#  3. staff                    staff per 1000 students
#  4. expend                   expend. per stud., $
#  5. salary                   avg. teacher salary, $
#  6. benefits                 avg. teacher benefits, $
#  7. droprate                 school dropout rate, perc
#  8. gradrate                 school graduation rate, perc
#  9. math10                   perc studs passing MEAP math
# 10. sci11                    perc studs passing MEAP science
# 11. totcomp                  salary + benefits
# 12. ltotcomp                 log(totcomp)
# 13. lexpend                  log of expend
# 14. lenroll                  log(enroll)
# 15. lstaff                   log(staff)
# 16. bensal                   benefits/salary
# 17. lsalary                  log(salary)

# Main variables are lnchprg and math10
#
# This is school-level data


# Results -----------------------------------------------------------------

# Simple linear regression
summary(lm(math10 ~ lnchprg, data=lunchData))

# What is going on??

# [Hint: tell me the omitted variables / selection story]

# One variable to include is the class size, as you may have concluded
# from previous analyses on the relationship between 
# class size and student performance.

# Let us also include enrollment, and staff, to control
#  for the size of the school.
summary(lm(math10 ~ lnchprg + enroll + staff, data=lunchData))

# This does not seem to make much of a difference.
#
# The main confounding factor here is that
#   schools in poor neighbourhoods will do more
#   poorly, and will also have increased
#   access to the lunch program
#
# One way to control for this is to take into account
#   the amount of money the school has available.
#
# This could be proxied by the expenditure per student
#   and the salary of the parents.
summary(lm(math10 ~ lnchprg + staff + salary + benefits + expend, data=lunchData))

# Q. Can you experiment and obtain the desired sign?

# See below for a possible explanation.

















# My take on this:
#  the intervention only works for 
#    the lower tail.
#
# Let's look at schools in the lowest quartiles
#    of faculty salaries.
summary(lm(math10 ~ lnchprg + staff, 
           data=lunchData, 
           subset=(salary<25000)))

# Further analysis --------------------------------------------------------

# Science score: 
summary(lm(sci11 ~ lnchprg + staff + salary + benefits + expend, data=lunchData))

# Perhaps the lunch program is keeping 
#   low-performance students in school?
summary(lm(gradrate ~ lnchprg + staff, data=lunchData))

#
# Q: What is your evaluation of the program?
# Q: Can you formulate an interaction model that
#         demonstrates what is going on?
#