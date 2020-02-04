# package about CART
library(rpart)

BestTree <- function(X,Y){

  TreeMAX = rpart(Y~.,data=X,control = rpart.control(cp=10^-9,minsplit=2))
  # plotcp(TreeMAX)
  CPs=TreeMAX$cptable
  smallest_xerror = which(CPs[,'xerror'] == min(CPs[,'xerror']))
  oneSE = CPs[smallest_xerror,'xerror']+CPs[smallest_xerror,'xstd']
  CPBestTree = head(CPs[CPs[,'xerror']<=oneSE,],1)[,'CP']

  # BestTree = rpart(Y~.,data=X,control = rpart.control(cp=CPBestTree,minsplit=2))
  BestTree = prune(TreeMAX,cp=CPBestTree)
}

data(mtcars)

X = mtcars[,-1]
Y = mtcars[,1]

Tree = BestTree(X,Y)
plot(Tree)
text(Tree)
printcp(Tree)

