# get set working directory
# getwd()
# setwd("C:/.../R-part2")

# set working directory to current one
library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# Read table from txt file
B=read.table('data2.txt')
B
# display nb of occurences per value
table(B)


v=var(B)*999/1000
v
# mean needs a matrix: as.matrix()
m=mean(as.matrix(B))
m

# an estimator of p
phat=1-v/m
# an estimator of n
nhat=m/phat

phat
nhat

#nhat should be integer --> nhat=11; --> needs to recalculate phat = m/nhat
nhat=11
phat=m/nhat

