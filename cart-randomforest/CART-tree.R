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


