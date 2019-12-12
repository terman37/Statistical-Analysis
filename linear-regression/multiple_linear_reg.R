
# Generate multiple variables
A=matrix(0,nrow=500,ncol=4)
A[,1]=rexp(500,0.4)
A[,2]=rnorm(500,3,0.5)
A[,3]=rpois(500,0.8)
A[,4]=runif(500,-3,3)
# and a response
Y=3+2*A[,1]-5*A[,2]+7*A[,4]+rnorm(500)

# Model
L=lm(Y~.,data=as.data.frame(A))
L
# Check details
summary(L)

#always verify normality of the noise !
plot(L)
hist(L$residuals,freq = FALSE)





B=cbind(A[,1],A[,2],A[,4])
LB=lm(Y~.,data=as.data.frame(B))
LB
summary(LB)

# build the X matrix (Y = XB + U)
X = cbind(rep(1,50),A)
X

