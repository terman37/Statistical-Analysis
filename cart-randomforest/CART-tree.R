# package about CART
library(rpart)

#####################################################################################
# use sample dataset for classification
data(iris)

# Tree to explain species by all other columns
Tree1 = rpart(iris$Species~.,data=iris[,-5])
Tree1

# can construct tree using naming convention
# * at the end of the line means terminal node (leaves)

# 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
#   2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#   3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
#     6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
#     7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *

# 1) node number
# root / Petal.Length< 2.45 node question
# 150 number of observations
# 100 number of observations with errors (should be 0 for leaves when optimal) -- particular to classification
# setosa node value -- particular to classification
# (0.33333333 0.33333333 0.33333333) vector of "posterior probabilities" associated to each label -- particular to classification

plot(Tree1)

Tree1MAX = rpart(iris$Species~.,data=iris[,-5],control = rpart.control(cp=10^-9,minsplit=2))
plot(Tree1MAX)

plotcp(Tree1MAX)
summary(Tree1MAX)
printcp(Tree1MAX)
A=printcp(Tree1MAX)
alphaseq = A[,1]
K=length(alphaseq)
for (i in 1:(K-1)){
  T=prune(Tree1MAX,cp=alphaseq[K+1-i])
  plot(T)
  text(T)
}


#####################################################################################
# use sample dataset for regression
data(mtcars)

# Tree to explain species by all other columns
Tree2 = rpart(mtcars$mpg~.,data=mtcars[,-1])
Tree2

# 1) root 32 1126.04700 20.09062  
#   2) cyl>=5 21  198.47240 16.64762  
#     4) hp>=192.5 7   28.82857 13.41429 *
#     5) hp< 192.5 14   59.87214 18.26429 *
#   3) cyl< 5 11  203.38550 26.66364 *

# 1126.04700 errors -- should decrease when splitting node
# 20.09062 prediction associated to the node

# Maxiaml tree
help("rpart.control")
Tree2 = rpart(mtcars$mpg~.,data=mtcars[,-1],control = rpart.control(cp=10^-9,minsplit=2))
Tree2
plot(Tree2)

#####################################################################################


# learning and test samples
u=sample(1:150,120)
learning = iris[u,]
test = iris[-u,]

Tree = rpart(learning[,5]~.,data=learning[,-5],cp=0.02,minsplit=2)
plot(Tree)
text(Tree)

# Prediction

# vector of posterior possibilty
predict(Tree)
# predictions
predict(Tree, type = "class")

# Predict new observation
newobs=as.list(c(Sepal.Length=3.2,Sepal.Width=2.8,Petal.Length=3.2,Petal.Width=1.8))
predict(Tree, newdata=newobs, type = "class")

# Surrogate splits
summary(Tree)

# for leaves:
# Node number 13: 6 observations
# predicted class=virginica   expected loss=0.3333333  P(node) =0.05
#   class counts:     0     2     4
#   probabilities: 0.000 0.333 0.667 

# for intermediates nodes
# Node number 3: 81 observations,    complexity param=0.4303797
#   predicted class=virginica   expected loss=0.4938272  P(node) =0.675
#   class counts:     0    40    41
#   probabilities: 0.000 0.494 0.506 
#   left son=6 (44 obs) right son=7 (37 obs)
#   Primary splits:
#     Petal.Width  < 1.75 to the left,  improve=29.684240, (0 missing)
#     Petal.Length < 4.95 to the left,  improve=29.460860, (0 missing)
#     Sepal.Length < 6.35 to the left,  improve= 8.554024, (0 missing)
#     Sepal.Width  < 2.75 to the left,  improve= 2.616766, (0 missing)
#   Surrogate splits:
#     Petal.Length < 4.75 to the left,  agree=0.889, adj=0.757, (0 split)
#     Sepal.Length < 6.35 to the left,  agree=0.753, adj=0.459, (0 split)
#     Sepal.Width  < 3.05 to the left,  agree=0.679, adj=0.297, (0 split)

# nb of surrogate split max = nb of explanatory variables - 1
# max nb of surrogate can be set as a parameter
# 
# first in primary split --> best split
# no use for others
# surrogate split: if first primary split value is missing 
# then use first surrogate (so splitting would be quite the same when removing primary split value from data)
 