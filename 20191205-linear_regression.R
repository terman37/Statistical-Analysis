
X=runif(100,-3,3)
Y=3-2*X*rnorm(100)

plot(X,Y)
L=lm(Y~X)

names(L)

summary(L)

L$coefficients

# Value of residual for each point
R=L$residuals
# is the noise Gaussian ?
hist(R,freq=FALSE)
# to check if noise is centered: residuals vs fitted
# quantile-quantile plot: standardized residuals (observed quantiles)
plot(L)

# = residual standart error in summary(L)
sigmahatn2=sum(L$residuals^2)/98
sqrt(sigmahatn2)


# Predictions Yi hat
L$fitted.values

# Rank of the Matrix
L$rank




