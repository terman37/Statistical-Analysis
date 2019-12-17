
# ****************************************************************
# general linear model
# install.packages('glmnet')
library(glmnet)

# alpha = 0 --> Ridge regression

x=matrix(rnorm(100*200),100,20)
y=rnorm(100)
fit1=glmnet(x,y,alpha=0)
plot(fit1)

x=matrix(rnorm(100*10),ncol=10)
y=rnorm(100)
fit1=glmnet(x,y,alpha=0)

# trajectories of different coefficient
plot(fit1)
# Ridge regression is not a way to perform variable selection

# Lasso
# allow from a given lambda to select variables
fit2=glmnet(x,y,alpha=1)
plot(fit2)
# Df = degree of freedom (nb of variable)
print(fit2)

# cross validation (cv.glmnet)
fit2cv = cv.glmnet(x,y)
names(fit2cv)

# best value of lambda (cross validation error smallest)
fit2cv$lambda.min
# better lambda with 1se (std error) 
fit2cv$lambda.1se

# using cross validation lambda
fit2=glmnet(x,y,alpha=1,lambda=fit2cv$lambda.1se)
fit2
coefficients(fit2)

# *************************************************************
# Other example
X=matrix(0,ncol=5,nrow=100)
X[,1]=runif(100,-4,3)
X[,2]=rexp(100,4)
X[,3]=rexp(100,0.02)
X[,4]=rpois(100,10)
X[,5]=rf(100,2,5)
Y=-2+3*X[,2]-5*X[,1]+X[,4]+rnorm(100)

fitcv = cv.glmnet(X,Y)
plot(fitcv)
fitcv$lambda.1se
# biased solution...
fit = glmnet(X,Y,lambda = fitcv$lambda.1se)
coefficients(fit)
# better used Lasso to select variables
# then classical linear ordinary least squared
# for remaining variables
reg = lm(Y~.,data=as.data.frame(X[,c(1,2,4)]))
reg
