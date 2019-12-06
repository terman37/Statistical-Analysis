
# Study the variation over K iterations
# of estimator 
# using n randomly generated observations using EXPONENTIAL distribution of parameter lambda


fluc <- function(n,lambda,K)
{
  # L empty vector
  L=c()
  for (i in 1:K){
    # generates dataset of EXPONENTIAL distribution parameter lambda, n observations
    A=rexp(n,lambda)
    # add estimator: 1/mean(A) in L
    L=c(L,1/mean(A))
  }
  # return L
  fluc=L
}

# 100 observations
# exponential parameter lambda = 4
# 50 estimators generated
Est=fluc(100,4,50)
boxplot(Est)

# more observations -> better estimator
Est=fluc(1000,4,50)
boxplot(Est)

