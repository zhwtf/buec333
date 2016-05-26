############################################
## SIMULATIONS #############################
############################################

# This is how to draw a random sample from the
# population, if it has a normal distribution
# with mean mu and sd sigma

mu <- 3
sigma <- 2

# In the world of the computer, whoever writes the script is
# all-knowing. That includes population parameters.

n <- 10
x <- rnorm(n,mean=mu,sd=sigma)
# x <- runif(n,0,1)

x

# Another sample
x <- rnorm(n,mean=mu,sd=sigma)
x

# Compute our estimators
xbar <- mean(x)
xtilde <- 1/(n-1)*sum(x)
x1 <- x[1]

c(xbar,xtilde,x1)

# Now, let's do this many times!

S <- 100
xbar <- numeric(S)
xtilde <- numeric(S)
x1 <- numeric(S)
for(s in seq(1,S)) {
  x <- rnorm(n,mean=mu,sd=sigma)
  xbar[s] <- mean(x)
  xtilde[s] <- 1/(n-1)*sum(x)
  x1[s] <- x[1]
}

# Put this in a data frame, and let's see what happened
simresults <- data.frame(xbar,xtilde,x1)

require(ggplot2)
qplot(xbar,data=simresults)

qplot(x1,data=simresults)

# Here is another way to visualize the results:
# Make a kernel density plot: this is basically a histogram,
# evaluated at every point.
# It is an estimate of the density function.

simplot <- ggplot(data=simresults)
simplot <- simplot + geom_density(aes(x=xbar))
simplot

simplot <- simplot + geom_density(aes(x=x1),col="red")
simplot

# They are both centered. What about xtilde?

simplot <- simplot + geom_density(aes(x=xtilde),col="blue")
simplot

# It is to the right! See what happens when we adjust n...
