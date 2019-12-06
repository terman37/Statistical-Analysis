#
# COMPUTE CI FOR SIG1/SIG2
#

alpha=0.05

confvar <- function(n,m,sig1,sig2,alpha){
  
  X=rnorm(n,0,sqrt(sig1))
  Y=rnorm(m,0,sqrt(sig2))
  sighat1=var(X) # empirical variance of X
  sighat2=var(Y) # empirical variance of Y
  
  f1 = qf(alpha/2,n-1,m-1)
  f2 = qf(1-alpha/2,n-1,m-1)
  
  # CI
  confvar = c(sighat1/sighat2*1/f2,sighat1/sighat2*1/f1)
  
}

R2=confvar(20,25,1,1.5,alpha)
R2

R3=confvar(500,500,1,1.5,alpha)
R3

