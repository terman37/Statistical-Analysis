

#


x=seq(-1,5,0.01)

fx=0*(x<0)+(x^2/8)*(x>=0)*(x<2)+(-x^2/8+x-1)*(x>=2)*(x<4)+1*(x>=4)

plot(x,fx,type='l',col='red')

# 50000 simulationsof a uniform distribution on [0,1]
u=runif(50000)

Y=sqrt(8*u)*(u<=1/2)+(4-sqrt(8-8*u))*(u>1/2)

min(Y)
max(Y)

hist(Y,freq=FALSE,breaks=50)