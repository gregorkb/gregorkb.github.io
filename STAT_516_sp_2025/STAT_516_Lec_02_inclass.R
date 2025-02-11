# STAT_516_Lec_02_inclass
rm(list=ls())
link <- url("https://people.stat.sc.edu/gregorkb/data/hrbc.csv")
hrbc <- read.csv(link)
head(hrbc)


plot(hem ~ rbc, data = hrbc)
Y <- hrbc$hem
x <- hrbc$rbc

lm_out <- lm(Y~x)
plot(lm_out,which = 1)

b1hat <- cor(x,Y)*sd(Y)/sd(x)
b0hat <- mean(Y) - b1hat * mean(x)

abline(b0hat,b1hat,col="red")

Yhat <- b0hat + b1hat * x
# points(Yhat~x,col="red")
ehat <- Y - Yhat

n <- length(x)
sgsqhat <- sum(ehat^2)/(n-2)
sgsqhat


alpha <- 0.05
tval <- qt(1-alpha/2,n-2)
Sxx <- sum((x - mean(x))^2)
lo <- b1hat - tval * sqrt(sgsqhat) / sqrt(Sxx)
up <- b1hat + tval * sqrt(sgsqhat) / sqrt(Sxx)
c(lo,up)

lm_out <- lm(Y ~ x)
confint(lm_out,level=.99)

plot(lm_out,which=4)

# testing H0: b1 = 0

Tstat <- b1hat / sqrt(sgsqhat/Sxx)
Tstat
pval <- 2*(1- pt(abs(Tstat),n-2))
pval


lm_out <- lm(Y ~ x)
summary(lm_out)

# CI for b0 + b1*xnew

alpha <- 0.05
xnew <- 5.5
se <- sqrt(sgsqhat)*sqrt(1/n + (xnew - mean(x))^2/Sxx)
tval <- qt(1-alpha/2,n-2)
lo <- b0hat + b1hat*xnew - tval*se
up <- b0hat + b1hat*xnew + tval*se
c(lo,up)

# PI for Ynew for the value xnew

alpha <- 0.05
xnew <- 5.5
se <- sqrt(sgsqhat)*sqrt(1 + 1/n + (xnew - mean(x))^2/Sxx)
tval <- qt(1-alpha/2,n-2)
lo <- b0hat + b1hat*xnew - tval*se
up <- b0hat + b1hat*xnew + tval*se
c(lo,up)


# predict()

lm_out <- lm(Y~x)
xnew <- 5.5
predict(lm_out,newdata = data.frame(x = xnew),int = "pred")


# compute the ANOVA table

Y <- hrbc$hem
x <- hrbc$rbc

b1hat <- cor(x,Y)*sd(Y)/sd(x)
b0hat <- mean(Y) - b1hat * mean(x)

Yhat <- b0hat + b1hat * x

SStot <- sum((Y - mean(Y))^2)
SSreg <- sum((Yhat - mean(Y))^2)
SSerr <- sum((Y - Yhat)^2)

MSreg <- SSreg / 1
MSerr <- SSerr / (n-2)

Fstat <- MSreg / MSerr
Fstat

pval <- 1 - pf(Fstat,1,n-2)
pval

# ANOVA table with lm function

lm_out <- lm(Y~x)
anova(lm_out)

summary(lm_out)

### Abalone data

csv <- url("https://people.stat.sc.edu/gregorkb/data/abalone.csv")
abalone <- read.csv(csv,col.names = c("Sex",
                                      "Length",
                                      "Diameter",
                                      "Height",
                                      "Whole_Wt",
                                      "Shucked_Wt",
                                      "Viscera_Wt",
                                      "Shell_Wt",
                                      "Rings"))
Y <- abalone$Shucked_Wt
x <- abalone$Length
n <- length(Y)

plot(Y~x)

lm_out <- lm(Y ~ x)
plot(lm_out,which = 1)

x3 <- x^3
plot(Y ~ x3)
lm3_out <- lm(Y ~ x3)
plot(lm3_out, which = 1)

# try natural log
logY <- log(Y)
logx <- log(x)

plot(logY ~ logx)
lmlog_out <- lm(logY ~ logx)
plot(lmlog_out,which =1)
summary(lmlog_out)

# two-sample t-test

link <- url("https://people.stat.sc.edu/gregorkb/data/hrbc.csv")
hrbc <- read.csv(link)
head(hrbc)

boxplot(hrbc$hem ~ hrbc$diag)

t.test(hrbc$hem ~ hrbc$diag,var.equal = TRUE)

Y <- hrbc$hem
x <- ifelse(hrbc$diag == "appendicitis",1,0)
lm_out <- lm(Y~x)
summary(lm_out)
  
