# STAT_516_Lec_03_inclass


link <- url("https://people.stat.sc.edu/gregorkb/data/KNLIcp.txt")
cp <- read.table(link,col.names=c("rent","age","optx","vac","sqft"))
cp$sqft <- cp$sqft/10000 # rescale sqft
head(cp)
plot(cp)


Y <- cp$rent
X <- cbind(1,cp$age,cp$optx,cp$vac,cp$sqft)
bhat <- solve(t(X) %*% X) %*% t(X) %*% Y
bhat

Yhat <- X %*% bhat
ehat <- Y - Yhat
n <- nrow(X)
p <- ncol(X) - 1
sgsqhat <- 1/(n - (p + 1)) * sum(ehat^2)
sgsqhat

Om <- solve( t(X) %*% X / n)
om <- diag(Om)

alpha <- 0.05
tval <- qt(1 - alpha/2,n-(p+1))

lo <- bhat - tval * sqrt(sgsqhat) * sqrt(om / n)
up <- bhat + tval * sqrt(sgsqhat) * sqrt(om / n)
cbind(bhat,lo,up)

Tstat <- bhat / (sqrt(sgsqhat) * sqrt(om / n))
Tstat

lm_out <- lm(rent ~ age + optx + vac + sqft, data = cp)
summary(lm_out)
confint(lm_out)

xnew <- data.frame(age = 10, optx = 7, vac = 0.20, sqft = 8)
predict(lm_out,newdata = xnew, int = "conf")
predict(lm_out,newdata = xnew, int = "pred")
