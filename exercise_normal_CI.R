
mu = 2
sig2 = 1.7
n=500
alpha=0.05

# create 500 observations ~ N(mu,sig)
A=rnorm(n,mu,sqrt(sig2))

# compute CI
# var(A) = sigma hat square (n-1) observations

lowerbound = mean(A) - sqrt(var(A))/sqrt(n)*qt(1-alpha/2,n-1)
upperbound = mean(A) + sqrt(var(A))/sqrt(n)*qt(1-alpha/2,n-1)

c(lowerbound,upperbound)

# repeat 50 times
visu<-function(k,n,mu,sig2){
  M=matrix(data=0,ncol=2,nrow=k)
  for (i in 1:k) {
    A=rnorm(n,mu,sqrt(sig2))
    # CI in gaussian settings
    lowerbound = mean(A) - sqrt(var(A))/sqrt(n)*qt(1-alpha/2,n-1)
    upperbound = mean(A) + sqrt(var(A))/sqrt(n)*qt(1-alpha/2,n-1)
    M[i,1]=lowerbound
    M[i,2]=upperbound
  }
  N=sum((M[,2]<2)+(M[,1]>2))
  visu=list(interval=M,count=N)
}

k=50
Z=visu(k,n,mu,sig2)
Z$count

plot(Z$interval)
