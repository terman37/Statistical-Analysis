
X=matrix(data=c(rep(1,100),
                c(rep(1,20),rep(0,80)),
                c(rep(0,20),rep(1,25),rep(0,55)),
                c(rep(0,45),rep(1,20),rep(0,35)),
                c(rep(0,65),rep(1,10),rep(0,25)),
                c(rep(0,75),rep(1,25))),ncol=6)

B = matrix(data = c(4,5,-2,7,.2,.1),ncol=1)

Y= X %*% B + matrix(data = rnorm(100),ncol=1)

xf = factor(c(rep('A',20),rep('B',25),rep('C',20),rep('D',10),rep('E',25)))
# vizualization
boxplot(Y~xf)

# use lm with factor (not vector)
mod=lm(Y~xf)
# intercet = mu
# label A is the reference set = 0 (not shown in coefficients)
mod

n1 = sum(xf=='A')
muhat = 1/n1*sum(Y*(xf=='A'))

summary(mod)
# we prefer to use anova 
# 
anova(mod)
