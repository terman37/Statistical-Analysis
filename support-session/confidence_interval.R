
#
lambda = 4.5
data = rexp(1000,lambda)

# an estimator of lambda
lambdahat=1/mean(data)

xbar = mean(data)

# confidence interval for lambda
alpha = 0.05
a = - qnorm(alpha/2)



#install.packages('Rmisc')
library(Rmisc)
ci = CI(data,ci=0.95)
ci
lambdaci = 1/ci
lambdaci
