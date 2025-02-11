
n <- 200
Y <- rnorm(n)

kde_gaussian <- function(x,Y,h) 1/(n*h) * sum(dnorm((Y-x)/h))
  
h <- .5
x <- seq(-5,5,length=300)
fx <- sapply(x,FUN=kde_gaussian, Y = Y, h = h)
plot(fx~x,type="l")
lines(dnorm(x)~x,lty=2)

# crossvalidation
hh <- seq(.05,1,by = 0.01)
CV <- numeric(length(hh))
for(j in 1:length(hh)){
  
  fx <- sapply(x,FUN=kde_gaussian, Y = Y, h = hh[j])
  A <- sum(fx^2)*(x[2]-x[1])
  
  B <- 0
  for(i in 1:n) B <- B + kde_gaussian(Y[i],Y[-i],hh[j]) / n
  
  CV[j] <- A - 2*B

}

plot(CV~hh)
hcv <- hh[which.min(CV)]

# plot kde with crossvalidation choice of h
fx <- sapply(x,FUN=kde_gaussian, Y = Y, h = hcv)
plot(fx~x,type="l")
lines(dnorm(x)~x,lty=2)
