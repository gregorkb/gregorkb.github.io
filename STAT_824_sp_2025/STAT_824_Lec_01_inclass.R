# Brownian motion

n <- 10000
Z <- rnorm(n)
B <- c(0,cumsum(Z) / sqrt(n))
t <- c(0:n)/n
plot(B~t ,type="l") 

# bridge

B0 <- B - t * B[n+1]
lines(B0 ~ t, col = "red")
abline(h = 0,lty = 3)


S <- 10000
sB0 <- numeric(S)
for(s in 1:S){
    
  Z <- rnorm(n)
  B <- c(0,cumsum(Z) / sqrt(n))
  t <- c(0:n)/n
  B0 <- B - t * B[n+1]
  sB0[s] <- max(abs(B0))

}

quantile(sB0,.95)
M <-1000
x <- 1.349
1 - 2 * sum( (-1)^(0:(M-1))*exp(-2*c(1:M)^2*x^2))
