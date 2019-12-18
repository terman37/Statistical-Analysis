# *********************************************************************************


Ble = read.table('Dataset_Ble.txt',header=TRUE,sep=';')
# Initial exploration
dim(Ble)
head(Ble)

attach(Ble)

# *********************************************************************************
# one-way anova influence of variete on rdt

boxplot(rdt~variete,col='yellow')

VLM = lm(rdt~variete)
summary(VLM)
plot(VLM,2)
# residuals seems evenly distributed / qqplot quite a line --> noise gaussian

AVLM = anova(VLM)
AVLM
# p-value veryy small < 0.05 --> H1 --> all mu_i not equal 
# --> there is an influence of variete on rdt

install.packages('car')
library(car)
leveneTest(rdt~variete)
# pvalue small --> 

# *********************************************************************************
# one-way anova influence of phyto on rdt

boxplot(rdt~phyto,col='red')

PLM = lm(rdt~phyto)
summary(PLM)
plot(PLM,2)
# residuals seems evenly distributed / qqplot quite a line --> noise gaussian

APLM = anova(PLM)
APLM
# p-value very big > 0.05 --> H0 --> all mu_i considered equal 
# --> there is no influence of phyto on rdt
leveneTest(rdt~phyto)

# *********************************************************************************
# two-way anova influence of phyto and variete on rdt

boxplot(rdt ~ phyto * variete, col=c('green','blue'))

# model with interraction effect between phyto and variete
modelint = aov(rdt ~ phyto * variete)
# check residuals are gaussian
plot(modelint,2)
summary(modelint)
TukeyHSD(modelint)
# p-value big for phyto:variete --> no interraction effect from phyto and variete

leveneTest(rdt~phyto * variete)

# additive model (variete and phyto independent)
modeladd = aov(rdt ~ phyto + variete)
summary(modeladd)
TukeyHSD(modeladd)
# seems phyto is not influent whereas variete is.

