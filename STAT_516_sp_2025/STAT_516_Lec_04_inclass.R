link <- url("https://people.stat.sc.edu/gregorkb/data/KNLIcp.txt")
commprop <- read.table(link,col.names=c("rent","age","optx","vac","sqft"))
commprop$sqft <- commprop$sqft/10000 # rescale sqft
head(commprop)

n <- nrow(commprop)
p <- ncol(commprop) - 1
  
lm_out <- lm(rent ~ age + optx + vac + sqft,data = commprop)
summary(lm_out)

1 - pf(26.76,4,76)

# Full-reduced model F test for optx and vac

lm_full <- lm(rent ~ age + optx + vac + sqft,data = commprop)
lm_red <- lm(rent ~ age + sqft,data = commprop)

SSE_full <- sum(lm_full$residuals^2)
SSE_red <- sum(lm_red$residuals^2)

num <- (SSE_red - SSE_full) / 2 # we are taking out 2 variables
denom <- SSE_full / (n - (p+1))
Fstat <- num / denom 
Fstat

qf(.95,2,76)

## Full reduced model F test to see if "vac" is important


lm_full <- lm(rent ~ age + optx + vac + sqft,data = commprop)
lm_red <- lm(rent ~ age + optx + sqft,data = commprop)

SSE_full <- sum(lm_full$residuals^2)
SSE_red <- sum(lm_red$residuals^2)

num <- (SSE_red - SSE_full) / 1 # we are taking out onle one variable
denom <- SSE_full / (n - (p+1))
Fstat <- num / denom 
Fstat

1 - pf(0.325,1,76)
