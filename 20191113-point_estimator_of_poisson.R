
# Study the variation over K iterations
# of estimator 
# using n randomly generated observations using POISSON distribution of parameter lambda


fluc <- function(n,lambda,K)
{
  # L empty vector
  L=c()
  for (i in 1:K){
    # generates dataset of POISSON distribution parameter lambda, n observations
    A=rpois(n,lambda)
    # add estimator: mean(A) in L
    L=c(L,mean(A))
  }
  # return L
  fluc=L
}

# 200 observations
# poisson parameter lambda = 5
# 1000 estimators generated
Est=fluc(200,5,1000)
hist(Est,freq=FALSE,breaks = 20)
boxplot(Est)



# Other way
# Matrix with 200 col and 1000 rows filled with Poisson (5) observations
M=matrix(data=rpois(200*10000,5),ncol=200)
# Compute row by row the empirical mean
# N is composed of 1000 observations of lambda_hat_n2
N=apply(M,1,mean)
hist(N, freq=FALSE) # looks like a gaussian (central limit theorem) parameters: 5 , 5/200




