# Outliers

beta0 <-  1
beta1 <-  2
n <- 100

X <- runif(n,0,1)
u <- rnorm(n,0,1)
Y <- beta0 + beta1*X + u

df_outlier <- data.frame(X,Y)

summary(lm(Y~X,data=df_outlier))



df_outlier$Y[1] <- df_outlier$Y[1] + 100
summary(lm(Y~X,data=df_outlier))
