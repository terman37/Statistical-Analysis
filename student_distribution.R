

# Student distribution
# 20191125

t = seq(-10,10,0.05)

# density function of student distribution (degree of freedom 1)
y1 = dt(t,1)

# density function of student distribution (degree of freedom 5)
y5 = dt(t,5)

# density function of student distribution (degree of freedom 5)
y10 = dt(t,10)

# density function of student distribution (degree of freedom 5)
y50 = dt(t,50)

#max for y axis
ym=max(y1,y5,y10,y50)

plot(t,y1,xlim = c(-10,10),ylim=c(0,ym),type = 'l',col='red')
par(new=TRUE)
plot(t,y5,xlim = c(-10,10),ylim=c(0,ym),type = 'l',col='blue')
par(new=TRUE)
plot(t,y10,xlim = c(-10,10),ylim=c(0,ym),type = 'l',col='green')
par(new=TRUE)
plot(t,y50,xlim = c(-10,10),ylim=c(0,ym),type = 'l',col='yellow')
par(new=TRUE)

# std gaussian (quite the same for big degree of freedom)
yn=dnorm(t,0,1)
plot(t,yn,xlim = c(-10,10),ylim=c(0,ym),type = 'l',col='black')
par(new=TRUE)

