# STAT_824_Lec_04_inclass.R

n <- 200
X <- runif(n)
Y <- 5*X *sin(2*pi* (1 + X)^2) - 2.5*X + rnorm(n)

data <- list(X = X,
             Y = Y)
plot(Y~X, data = data)


# Make a NW estimator

ker <- function(x){3/4*(1 - x^2) *((x < 1) & (x > - 1)) }

NW <- function(x,data,h){
  
  X <- data$X
  Y <- data$Y
  Wx <- ker((X - x)/h) / sum(ker((X - x)/h))
  mx <- sum(Wx*Y)
  return(mx)

}

h <- .1
x <- seq(0,1,length=200)
mx <- sapply(X = x,FUN = NW,data = data, h = h)

plot(Y~X, data = data, col ="gray")
lines(mx~x,col="red",lwd = 3)

# LP 

LP <- function(x,data,h,l){
    
  Ux <- matrix(0,n,l+1)
  Ux[,1] <- 1
  
  if( l > 0){
    
    for(k in 1:l){
      
      Ux[,1 + k] <- (X - x)^k/factorial(k)
      
    }
    
  }
  
  Kx <- diag(ker((X - x)/h))
  Lx <- Kx %*% Ux %*% solve( t(Ux) %*% Kx %*% Ux)
  Wx <- Lx[,1]
  mx <- sum(Wx*Y)
 
  return(mx) 
}

l <- 3

mx <- sapply(X = x,FUN = LP,data = data, h = h , l = l)
lines(mx~x,col="blue",lwd = 3)
