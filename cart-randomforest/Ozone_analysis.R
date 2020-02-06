
# load dataset
  setwd("C:/MY_DATAS/MyGit/Statistical-Analysis/cart-randomforest")
  ozone = read.table('Dataset_ozone.txt',header=TRUE,sep=';',dec = ",")

# imports
  # install.packages('ggplot2')
  library(ggplot2)

# First look
  dim(ozone)
  head(ozone)
  summary(ozone[c('maxO3','T12')])

# 1-Task (simple linear model)
# try to explain maxO3 by T12

  ggplot(ozone,aes(x=T12,y=maxO3)) + geom_point() 
  # plot(ozone$maxO3,ozone$T12)
  
  # looks like linear relationship
  #  Create model
  reg_simpl = lm(maxO3~T12,data = ozone)
  # Yi = B0 + B1 * xi + Ei
  # estimation of B0 and B1
  reg_simpl
  
  summary(reg_simpl)
  # look at multiple R-squared --> should be close to 1 to be very good
  # check if noise is gaussian:
  # - symmetry of Residuals (at begining in summary) / avg close to 0
  # - normal QQplot
  plot(reg_simpl,2)
  # --> noise seems gaussian
  # --> can use all output of summary function
  summary(reg_simpl)
  # F-Statistics
  # H0: B1 = 0 / H1: B1 != 0
  # pvalue very small (less than 0.05) --> H1 --> linear model ok
  
  # vizualize regression line
  ggplot(ozone,aes(x=T12,y=maxO3)) + 
    geom_point() + 
    stat_smooth(method = 'lm',se=FALSE) 
    
  # can check quality of model (all points should be on the red line)
  maxO3_adjusted = reg_simpl$fitted.values
  ggplot(ozone,aes(x=maxO3,y=maxO3_adjusted)) + 
    geom_point() + 
    geom_abline(intercept = 0,slope=1,color='red') + 
    xlab('a') + 
    ylab('b')

# 2-Task (multiple linear model)
# try to explain maxO3 by quantitative explanatory variables

  reg_multi = lm(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v,data = ozone)
  reg_multi
  
  # check correlation between explanatory variables
  cor(ozone[,3:12])
  # some strong correlations e.g. between Vx15 / Vx12 and T9 / T12 ...
  
  summary(reg_multi)
  # look at adjusted R-squared --> should be close to 1 to be very good
  # check if noise is gaussian in order to use tests displayed in summary
  plot(reg_multi,2)
  # --> noise seems gaussian
  # --> can use all output of summary function
  summary(reg_simpl)
  # F-Statistics
  # H0: B1,B2,... = 0 / H1: B1,B2,... != 0
  # pvalue very small (less than 0.05) --> H1 --> B1,B2,... != 0
  

# PCA (only on quantitative variables)
  ozonepca = ozone[,3:12]
  
  pca_ozone = princomp(ozonepca)              # non normalized PCA
  pcan_ozone = princomp(ozonepca,cor = TRUE)  # normalized PCA

  pca_ozone
  
  # explanations #########
  V=cov(ozonepca)
  ev = eigen(V)
  eigenvalues = ev$values
  sqrt(eigenvalues)
  ########################
  
  names(pca_ozone)
  pca_ozone$sdev      # sqrt of eigenvalues - default output
  pca_ozone$center    # mean of each variable
  pca_ozone$scale     # =1 in this case as not normalized PCA, else std dev of each variable
  pca_ozone$loadings  # definition of new variables (linear combination of old variables)
  
  plot(pca_ozone)
  # biplot(pca_ozone)
  pca_ozone
  # each value is sqrt of eigen values (=variance explained by the first new variable)
  s=pca_ozone$sdev
  cumsum(s^2/sum(s^2)) # proportion of variance explained by the first i variables
  
  # 2 first new variables explained about 96% of variance
  # only keeps those 2
  newdataset = pca_ozone$scores[,1:2]
  
  newdata = cbind(ozone$maxO3,newdataset)
  colnames(newdata)=c('maxO3','Comp1','Comp2')
  
  # now rerun linear model on new dataset
  reg_pca = lm(maxO3~Comp1+Comp2,data=as.data.frame(newdata))
  reg_pca  
  
  # Comp1 and Comp2 are linear combination of all variables
  # no variable selection
  
# Variable selection
  # backward selection on linear model (one by one removal)
  # T9 has the biggest pvalue --> suppress it
  reg_multi_2 = lm(maxO3~T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v,data = ozone)
  summary(reg_multi_2)
  # Vx12 has the biggest pvalue --> suppress it
  reg_multi_3 = lm(maxO3~T12+T15+Ne9+Ne12+Ne15+Vx9+Vx15+maxO3v,data = ozone)
  summary(reg_multi_3)
  # Ne15 has the biggest pvalue --> suppress it
  reg_multi_4 = lm(maxO3~T12+T15+Ne9+Ne12+Vx9+Vx15+maxO3v,data = ozone)
  summary(reg_multi_4)
  # Ne12 has the biggest pvalue --> suppress it
  reg_multi_5 = lm(maxO3~T12+T15+Ne9+Vx9+Vx15+maxO3v,data = ozone)
  summary(reg_multi_5)
  # T15 has the biggest pvalue --> suppress it
  reg_multi_6 = lm(maxO3~T12+Ne9+Vx9+Vx15+maxO3v,data = ozone)
  summary(reg_multi_6)
  # Vx15 has the biggest pvalue --> suppress it
  reg_multi_7 = lm(maxO3~T12+Ne9+Vx9+maxO3v,data = ozone)
  summary(reg_multi_7)
  
  # all pvalues are less than 5% --> stop the backward selection
  # keep this model.
  
# Create train / test sets

  u=1:112
  v=sample(u,90)
  ozone_learn = ozone[v,]
  ozone_test = ozone[-v,]
  
# Bagging
  library(ipred)  
  A = bagging(maxO3~.,data=ozone_learn,nbag=50)
  bagt = predict(A,newdata = ozone_test)  
  bagt  
  
  bagr <- function(ozone_learn,ozone_test){
    err=c()
    for (k in 1:50) {
      A = bagging(maxO3~.,data=ozone_learn,nbag=k)
      bagt = predict(A,newdata = ozone_test)
      e = 1/nrow(ozone_test) * sum((bagt-ozone_test$maxO3)^2)
      err = c(err,e)
    }
    bagr = err
  }
  
  plot(bagr(ozone_learn,ozone_test),type='l')
  
# RandomForest
  library(randomForest)
  
  rf_reg = randomForest(maxO3~.,data=ozone_learn,xtest=ozone_test[,-2],ytest=ozone_test[,'maxO3'],ntree=500,importance=TRUE)
  rf_reg
  
  names(rf_reg)
  rf_reg$importance
  
  # variable selection
  library(VSURF)
  
  vozone = VSURF(maxO3~., data=ozone)
  vozone$varselect.thres
  vozone$varselect.interp
  vozone$varselect.pred