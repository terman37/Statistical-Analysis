
X= runif(50,-7,7)
Y=5-6*X+rnorm(50,0,20)

plot(X,Y)

L=lm(Y~X)
L

# true coeficients
L$coefficients

# show model on plot
abline(L,col='red')

# predict function
help(predict.lm)

# Yhat of your observations
predict(L)

# Predictions for new values
# careful needs X named for new
new = seq(-3,3,0.1) # no
new = data.frame(X=seq(-20,20,0.5))
predict(L,new)

pred.w.plim <- predict(L, new, interval = "prediction") # for new objects
pred.w.clim <- predict(L, new, interval = "confidence") # for expectation
matplot(new$X, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

# green and red are for expectation of Y
# other are for Y

pred.w.plim <- predict(L, new, interval = "prediction")
pred.w.clim <- predict(L, new, interval = "confidence")
matplot(new$X,pred.w.clim,
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
matplot(new$X,pred.w.plim,
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")


plot(X,Y)
matlines(new$X, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")