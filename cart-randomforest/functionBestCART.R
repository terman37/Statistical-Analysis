# package about CART
library(rpart)

BestTree <- function(X,Y){

  DF = data.frame(X,Y)

  TreeMAX = rpart(DF$Y~.,data=X,control = rpart.control(cp=10^-9,minsplit=2))
  CPs=TreeMAX$cptable

  oneSE = CPs[nrow(CPs),'xerror']+CPs[nrow(CPs),'xstd']
  CPBestTree = head(CPs[CPs[,'xerror']<=oneSE,],1)[,'CP']

  BestTree = rpart(DF$Y~.,data=X,control = rpart.control(cp=CPBestTree,minsplit=2))
}

data(mtcars)

X = mtcars[,-1]
Y = mtcars[,1]

Tree = BestTree(X,Y)
plot(Tree)
text(Tree)
printcp(Tree)
