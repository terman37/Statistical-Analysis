# ****************************************************
#
# Analysis of Ozone.txt data
#
# check if wind (vent) has influence on concentration in ozone (maxO3)
#
# ****************************************************

ozone = read.table('ozone.txt',header = TRUE)
head(A)
dim(A)

attach(ozone)
# Global overview
summary(ozone[c('maxO3','vent')])

# Visual view (boxplot of variable maxO3 with repect of wind)
# pch (points shape) / cex (text size) options
# Check avalailable options in help(par)
boxplot(maxO3~vent,data=ozone,pch=15,cex=0.5,col='green')

# ****************************************************
# Analysis of variance with one factor
model = aov(maxO3~vent)
model

# ****************************************************
# Before analysing the outputs of test, 
# We need to check assumptions on the noise
# - Homoscedasticity (same variance): bartlett test, or levene test
# - Gaussianity: Normal QQPlot, or ks.test, or shapiro test
# - Independency: looking the experimenttion

# ****************************************************
# - Equality of the variances:

# 1st way: calculate all variances
var1 = var(maxO3[vent=='Est'])
var2 = var(maxO3[vent=='Nord'])
var3 = var(maxO3[vent=='Ouest'])
var4 = var(maxO3[vent=='Sud'])
# then compare pair by pair at level alpha / 6 (nb test = X! )
# ...

# 2nd way: levene test
summary(aov(abs(model$res)~vent))
# p-value big --> do not reject H0 --> variances can be considered equal

# 3rd way: bartlett test
bartlett.test(model$res~vent)
# p-value big --> do not reject H0 --> variances can be considered equal

# Conclusion: we can accept the equality of the variances

# ****************************************************
# - Gaussianity:

select.est = ozone[,'vent']=='Est'
select.nord = ozone[,'vent']=='Nord'
select.ouest = ozone[,'vent']=='Ouest'
select.sud = ozone[,'vent']=='Sud'

# Shapiro test
# here we are on 1 label 
# --> we can work directly on the response variable and not on the residuals
# --> if whole dataset -> consider residuals because response for all individuals are not iid
# Normality of the sample associated to label est
shapiro.test(ozone[select.est,'maxO3'])
# p-value big, --> H0 --> Gaussianity OK
shapiro.test(ozone[select.nord,'maxO3'])
shapiro.test(ozone[select.ouest,'maxO3'])
shapiro.test(ozone[select.sud,'maxO3'])


# QQnorm
qqnorm((ozone[select.est,'maxO3']))
# points seems to be aligned --> Gaussian


# Conclusion: the assumptions on the noise seems to be satisfied

# ****************************************************
# Finally
ozone.aov = aov(maxO3~vent)
summary(ozone.aov)
# p-value smaller than 5% --> there is an influence of the wind on the ozone concentration


# ****************************************************
# studentized version of residuals
res.ozone = rstudent(ozone.aov)
plot(res.ozone~vent)
summary(lm(maxO3~vent,data=ozone))

# constraint with reference cell on label 2
summary(lm(maxO3~C(vent,base=2),data=ozone))

# constraint on the sum of the mu_i = 0
summary(lm(maxO3~C(vent,sum),data=ozone))




# ************************************************************************************
X=data.frame(T1=c(5,8,7,7,10,8),
             T2=c(4,6,6,3,5,6),
             T3=c(6,4,4,5,4,3),
             T4=c(7,4,6,6,3,5),
             T5=c(9,3,5,7,7,6))

delai = stack(X)$values # transpose of Y
treatment = rep(c('T1','T2','T3','T4','T5'),each=6)
paste('T',1:5,sep = '')
treatment = factor(rep(c('T1','T2','T3','T4','T5'),each=6)) # the factor

plot(delai~treatment,col='green')

myaov = aov(delai~treatment)
summary(myaov)
# --> pvalue 0.01 --> influence

model = lm(delai~treatment)
model
summary(model)

# install.packages('gmodels')
library(gmodels)

cmat = rbind(" : 2 versus 3"=c(0,1,-1,0,0))
fit.contrast(myaov,treatment,cmat)

pairwise.t.test(delai,treatment,p.adjust="bonf")

TukeyHSD(myaov)
par(las=1) # horizontal labels
plot(TukeyHSD(myaov))
