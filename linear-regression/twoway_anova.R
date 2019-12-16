
X=matrix(data=c(rep(1,10),
                c(rep(1,3),rep(0,7)),
                c(rep(0,3),rep(1,3),rep(0,4)),
                c(rep(0,6),rep(1,4)),
                c(1,1,0,1,0,0,1,1,0,0),
                c(0,0,1,0,1,1,0,0,1,1),
                c(1,1,0,0,0,0,0,0,0,0),
                c(0,0,1,0,0,0,0,0,0,0),
                c(0,0,0,1,0,0,0,0,0,0),
                c(0,0,0,0,1,1,0,0,0,0),
                c(0,0,0,0,0,0,1,1,0,0),
                c(0,0,0,0,0,0,0,0,1,1))
                ,ncol=12)
X

B = matrix(data = c(1,2,3,4,2,2,1,2,1,3,1,2),ncol=1)

Y = X %*% B + rnorm(10)

F1 = factor(c(rep(1,3),rep(2,3),rep(3,4)))
F2 = factor(c(1,1,2,1,2,2,1,1,2,2))

# just using additive model (no cross effect)
mod1 = lm(Y~F1+F2)
# with cross effect
mod2 = lm(Y~F1+F2+F1*F2)

mod2

anova(mod2)


