# STAT_516_Lec_01_inclass.R

gr <- c(1.66, 1.61, 1.62, 1.69, 1.58, 1.43, 1.66,
        1.69, 1.58, 1.20, 1.52, 1.60, 1.55, 1.67,
        1.77, 1.50, 1.64, 1.54, 1.40, 1.36, 1.50,
        1.40, 1.35, 1.48, 1.64, 1.91, 1.70)

alpha <- 0.05
n <- length(gr)
xbar <- mean(gr)
s <- sd(gr)
s2 <- var(gr)

lower <- xbar - qt(1-alpha/2,n-1)* s / sqrt(n)
upper <- xbar + qt(1-alpha/2,n-1)* s / sqrt(n)

c(lower,upper)

lov <- (n-1)*s2 / qchisq(1-alpha/2,n-1) 
upv <- (n-1)*s2 / qchisq(alpha/2,n-1) 
c(lov,upv)

mu0 <- 1.618

Tstat <- (xbar - mu0) / (s /sqrt(n))
Tstat

qt(.975,n-1)
-qt(.95,n-1)

pt(Tstat,n-1)

t.test(gr,mu=mu0,conf.level = .99)

# sample size calculation
M <- 0.04
alpha <- 0.05
sigma <- 0.148

n <- ceiling( (qnorm(1 - alpha/2) * sigma / M )^2 )
n

# plot a power curve

alpha <- 0.05
sigma <- 0.148
n <- 20
mu0 <- 1.618
mu <- seq(1.55,1.75,length=500)
power <- 1 - pnorm(qnorm(1-alpha) - (mu - mu0)/(sigma/sqrt(n)))
plot(power ~ mu,type="l")
lines(power ~ mu, lty = 2)
abline(v=mu0, lty =3)
abline(h=alpha, lty = 3)



#
alpha <- 0.05
mu <- 1.65
gamma <- 0.80
mu0 <- 1.618
sigma <- 0.148
za <- qnorm(1-alpha)
zb <- qnorm(gamma)
n <- ceiling(sigma^2 *( za + zb)^2/(mu - mu0)^2)
n
