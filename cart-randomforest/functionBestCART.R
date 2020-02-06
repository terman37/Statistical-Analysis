
BestTree <- function(X,Y){
  # package about CART
  library(rpart)
  # generate max tree
  TreeMAX = rpart(Y~.,data=X,control = rpart.control(cp=10^-9,minsplit=2))
  # find cp corresponding to OneSE
  CPs=TreeMAX$cptable
  smallest_xerror = which(CPs[,'xerror'] == min(CPs[,'xerror']))
  oneSE = CPs[smallest_xerror,'xerror']+CPs[smallest_xerror,'xstd']
  CPBestTree = head(CPs[CPs[,'xerror']<=oneSE,],1)[,'CP']
  # Prune Tree and return best tree
  BestTree = prune(TreeMAX,cp=CPBestTree)
}

Bagging <- function(X,Y,nbtree=5,nbobs=30){
  bagtree = list()
  # generate nb trees
  for (k in 1:nbtree) {
    rows_selected = sample(1:nrow(X),nbobs,replace = TRUE)
    newX = X[rows_selected,]
    newY = Y[rows_selected]
    Treek = BestTree(newX,newY)
    bagtree = append(bagtree,list(Treek))
    names(bagtree)[k] <- paste("Tree",k)
  }
  # return collection of trees
  Bagging <- bagtree
}

data(mtcars)

X = mtcars[,-1]
Y = mtcars[,1]

Trees = Bagging(X,Y,nbtree=10,nbobs=50)

newobs=X[6,]
result = 0
for (k in 1:length(Trees)) {
  result = result + 1 / length(Trees) * predict(Trees[[k]], newdata=newobs)
}
result

# Tree = BestTree(X,Y)
# plot(Tree)
# text(Tree)
# printcp(Tree)

