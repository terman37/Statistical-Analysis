

cart <- function(learnX, learnY){
  library(rpart)
  # construction of maximal tree
  T = rpart(learnY~.,data=learnX,control=rpart.control(cp=10^-9,minsplit=2))
  # Pruning
  A = printcp(T)
  b = which(A[,4]==min(A[,4]))
  s = A[b,4]+A[b,5]
  s = unique(min(s))
  w = 1*(A[,4]<=s)
  v = which(w==1)
  r = v[1]
  cp = A[r,1]
  finalT = prune(T,cp=cp)
  cart = finalT
}

data(mtcars)

X = mtcars[,-1]
Y = mtcars[,1]

Tree = cart(X,Y)
plot(Tree)
text(Tree)
printcp(Tree)
