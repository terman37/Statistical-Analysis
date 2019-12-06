# *********************************************************************
# DETERMINE CONFIDENCE INDERVAL FOR DISTRIBUTION PARAMETER
# *********************************************************************

# set working directory to current one
library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

alpha=0.05

#***********************************************
# First sample:
  A = as.matrix(read.table("simu1.txt"))
  # look at the shape
  hist(A)
  
  # guess an exponential distribution
  lambdahat = 1 / mean(A)
  lambdahat
  
  # goodness-of-fit test - Kolmogorov-Smirnov
  ks.test(A,'pexp',lambdahat)
  # p-value > alpha --> we keep H0 (this is an exponential dist) --> we keep our guess
  # be careful, ks.test is sensitive to sample size (not use with small sample)

  # Confidence interval for lambda with confidence level 100*(1-alpha)%
  lowerbound = 1/mean(A) - 1/(sqrt(length(A))*mean(A))*qnorm(1-alpha/2)
  upperbound = 1/mean(A) + 1/(sqrt(length(A))*mean(A))*qnorm(1-alpha/2)
  
  c(lowerbound,upperbound)

  
#*********************************************** 
# Second sample:
  B = as.matrix(read.table("simu2.txt"))
  # look at the shape
  hist(B)
  # guess a normal distribution
  muhat = mean(B)
  sig2hat = var(B)
  n=length(B)
  # goodness-of-fit test - Kolmogorov-Smirnov
  ks.test(B,'pnorm',muhat,sqrt(sig2hat))
  
  # Confidence interval for mu
  lowerbound = muhat - sqrt(sig2hat)/sqrt(n)*qt(1-alpha/2,n)
  upperbound = muhat + sqrt(sig2hat)/sqrt(n)*qt(1-alpha/2,n)
  
  # Confidence interval for sig2
  lowerboundsig = (n-1)*sqrt(sig2hat)/qchisq(1-alpha/2,n-1)
  upperboundsig = (n-1)*sqrt(sig2hat)/qchisq(alpha/2,n-1)
  
#*********************************************** 
# Third sample:
  C = as.matrix(read.table("simu3.txt"))
  # look at the shape
  hist(C)
  
  # do the same...