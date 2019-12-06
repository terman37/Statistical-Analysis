
# With X ~ T(12): student distribution 12 degrees of freedom

# P( X < t ) = 0.2
# quantile function (inverse of the density function)
qt(0.2,12)


# P( -t < X < t ) = 0.01
# P( |X| > t ) = 0.99
# P ( X > t ) = 0.99 / 2 = 0.495
# P ( X < t ) = 0.505
qt(0.505,12)


# P( X < 0.259)
pt(0.259,12)

# functions exists for other distributions as well
# rt -> generate n random values    : rpois, rnorm...
# pt -> distrubution  P(X<a) = ???  : ppois, pnorm...
# qt -> quantile      P(X<???) = a  : qpois,qnorm...
# dt -> density       P(X=a) = ???  : dpois, dnorm...

dt(0,12)

rt(10000,12)
