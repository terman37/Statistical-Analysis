
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