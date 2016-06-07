# ================= #
# BUEC 333          #
# Basic exercises 2 #
# ================= #

# Load the data
df = read.csv('testdata.csv')

# Plot B agains A. You have seen ggplot in class. We will look at the basic plot() a little
# How do they appear to be related?
plot1 = plot(df$A, df$B)

# That's one ugly graph. Let's clean that up a bit
# Add labels
plot2 = plot(df$A, df$B, main = 'Fancy Plot Title', xlab = 'A', ylab = 'B')


# Still kind of ugly. Try ggplot2
require(ggplot2)
plot3 = ggplot(data=df, aes(x=A, y=B))
# View the plot
plot3
# ... Nothing. That's because so far we have only created the grid/canvas
# Add the points
plot4 = plot3 + geom_point()
plot4

# How are they related? Let's draw a straight line through the points
plot5 = plot4 + geom_smooth(method='lm') # 'lm' for linear model (OLS)
plot5

# The line looks 'fuzzy'. Those are the standard errors. Let's get rid off that
plot6 = plot4 + geom_smooth(method='lm', se=FALSE)
plot6


# What do we know about that line? Slope? Intercept?
# Let's derive it properly: OLS

# regress B on A
OLS1 = lm(B ~ A, data=df)

# How to get the results?
OLS1
# So we have an intercept (-39.33) and a slope (13.86)
# That's already enough to make some predictions. Pick some random A and corresponding B and see how good our model is
testA = df[42, 'A']
testB = df[42, 'B']
predictedB = -39.33 + 13.86 * testA
# What do we predict?
predictedB
# What is the actual value?
testB
# Meh...

# Before we move on, some better ways of looking at our results
summary(OLS1)
# You can also extract your estimated coefficients from the model
mycoeffs = OLS1$coefficients
predictedB2 = mycoeffs[1] + mycoeffs[2] * testA


# Looking at our scatter plot, a strqight line does not seem to be the right fit
plot6
# Let's try a quadratic function: B = A + A^2
df$A_sq = df$A^2
OLS2 = lm(B ~ A + A_sq, data=df)
summary(OLS2)

# How good is this model?
mycoeffs2 = OLS2$coefficients

predictedB3 = mycoeffs2[1] + mycoeffs2[2] * testA + mycoeffs2[3] * testA^2
# What do we predict?
predictedB3
# What is the actual value?
testB
# Much better

# A much faster way of doing this (especially when you have lots of variables)
predictedB4 = predict(OLS2, data.frame(A=testA, A_sq=testA^2))
predictedB4
predictedB4 == predictedB3

# You can also get the predicted results for your whole data set
df$B_hat = predict(OLS2)

plot7 = ggplot(data=df, aes(x=A, y=B_hat)) + geom_point()
plot7
