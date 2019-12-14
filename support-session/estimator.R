
n=100
data = rnorm(n,mean = 1.2, sd=2.3)

# estimator for mean 
xbar = mean(data)

# estimator for Standard deviation
sigmabar2 = sd(data)
sigmabar = sqrt(mean((data-xbar)^2))

# unbiased estimator
sigmanminus1 = sqrt(n/(n-1)) * sigmabar



# 
n=1000000
nexperiment = 1000
A = matrix(,nrow = nexperiment,ncol = 1)
for (i in 1:nexperiment){
  data = rnorm(n,mean=1.2,sd=2.3)
  xbar = mean(data)
  sigmabar = sqrt(mean((data-xbar)^2))
  sigmanminus1 = sqrt(n/(n-1)) * sigmabar
  A[i] = (n-1)*(sigmanminus1/2.3)^2
}
#hist(A,freq = FALSE,nclass = 50)
plot(density(A))

C = rchisq(nexperiment,n-1)
lines(density(C),col='red')


