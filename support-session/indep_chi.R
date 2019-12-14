
# result at the exam vs gender
#
# X = result at the exam (A,B,C,D)
# Y = gender (F/M)

#     F   M
# A   2   1
# B   4   7
# C   3   6
# D   1   3

data <- matrix(c(2,1,4,7,3,6,1,3),nrow = 4,byrow = TRUE)
data

px <- rowSums(data)
px

py <- colSums(data)
py

N<- sum(data)
N

empirical <- matrix(,nrow=4,ncol=2)
for (i in 1:dim(data)[1]){
  for (j in 1:dim(data)[2]){
    empirical[i,j]=px[i]*py[j]/N
  }
}
empirical

chi2 <- sum((data-empirical)^2/empirical)
chi2

pvalue = 1-pchisq(chi2,3)

# pvalue = 0.69 > 0.05 --> we cannot reject Ho

# does the same than previously
chisq.test(data)
