
# ***************************************************************************
# Y1 A1
Y = as.matrix(read.csv('Y1.txt',header = FALSE))
A = read.table('A1.txt')
head(A)

L=lm(Y~.,data=A)
summary(L)

# First look at Multiple R-Squared / adjusted R-squared
# -> stop here analysis with linear model ! 

# ***************************************************************************
# Y2 A1
Y = as.matrix(read.csv('Y2.txt',header = FALSE))
A = read.table('A1.txt')

L=lm(Y~.,data=A)
summary(L)
# R-squared is ok
# check if noise is gaussian setting
#   residuals median ~= 0 and almost symmetric
plot(L)
# first chart good repartition / symmetry vs horizontal axis
# look at QQ plot --> OK
summary(L)
# Fstatistic p-value small validate linear model
# cannot remove V2/V3/V4 at once because there may be correlation with others
# we should perform variable selection procedure
cor(A)

# ***************************************************************************
# Y3 A3
Y = as.matrix(read.csv('Y3.txt',header = FALSE))
A = read.table('A3.txt')

L=lm(Y~.,data=A)
summary(L)
# R-squared is ok
plot(L)
# Gaussian setting ok
cor(A)
# huge correlation between V1 and v6
# we can supress v1 or v6

# ***************************************************************************
# Y4 A4
Y = as.matrix(read.csv('Y4.txt',header = FALSE))
A = read.table('A4.txt')

L=lm(Y~.,data=A)
summary(L)
# residuals seems not symmetric
plot(L)
# Residuals vs Fitted chart - more points on left than right
# QQ plot --> lot of points far from line, difficult to consider gaussian noise
summary(L)
# not possible to use FStatistic (only for gaussian noise)
# p-values of coefficients cannot be used (not a student due to not gaussian)

hist(L$residuals,freq=FALSE,nclass=20)
# show noise is not gausssian
# Rsquared big so linear model ok, noise not gaussian, pvalue cannot be used
# use only least squared (no tests)
L

# variable selection
Lb=lm(Y~.,A[,c(1,5)])
summary(Lb)
