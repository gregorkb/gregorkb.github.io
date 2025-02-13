

n <- 200
X <- runif(n)
Y <- 5*X*sin((1 + X)^2 * 2 * pi) - 5/2 * X + rnorm(n)
plot(Y~X,col="gray")

library(splines)
K <- 10
r <- 3
u <- seq(0,1,length=K + 1)
urep <- c(rep(u[1],r),u,rep(u[K+1],r))
B <- splineDesign(knots = urep, x = X, ord = r + 1)
ahat <- solve(t(B)%*% B) %*% t(B)%*% Y
Yhat <- B %*% ahat
# points(Yhat~X,col="red")

apply(B,1,sum) # rows always sum to 1

# plot a nice line
x <- seq(0,1,length=200)
Bx <- splineDesign(knots = urep, x = x, ord = r + 1)
mxhat <- Bx %*% ahat
lines(mxhat ~ x,col="red")
