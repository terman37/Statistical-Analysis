
data = rnorm(1000000,mean = 3,sd = 2)

# histogram (occurences)
hist(data)
# density (occurences / total nb)
hist(data, freq = FALSE, nclass = 50)

# adding some uniform noise
data = data + runif(n = 1000000, min = -5,max = 5)

# empirical mean
Xn = mean(data)

# empirical variance
Sig2 = var(data)



# Shape of a chi square distrib
hist(rchisq(1000000,5),freq = FALSE,nclass = 50)

# Shape of a exponential distrib
hist(rexp(1000000,3),freq = FALSE,nclass = 50)

# Shape of a uniform distrib between 0 and 1
hist(runif(1000000),freq = FALSE,nclass = 50)

# Shape of a student distrib 
hist(rt(1000000,10),freq = FALSE,nclass = 50)


