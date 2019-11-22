
#example
A=matrix(1:10,ncol=2)
#sum by row
apply(A,1,sum)


M=matrix(data=runif(500000*100,0,3.5),nrow=5000)
Xbar=apply(M,1,mean)

hist(Xbar,freq=FALSE,breaks=20)