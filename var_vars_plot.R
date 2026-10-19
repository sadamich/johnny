https://cran.r-project.org/web/packages/vars/refman/vars.html#plot

library(vars)
data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
plot(var.2c)
## Diagnostic Testing
## ARCH test
archtest <- arch.test(var.2c)
plot(archtest)
## Normality test
normalitytest <- normality.test(var.2c)
plot(normalitytest)
## serial correlation test
serialtest <- serial.test(var.2c)
plot(serialtest)
## FEVD
var.2c.fevd <- fevd(var.2c, n.ahead = 5)
plot(var.2c.fevd)
## IRF
var.2c.irf <- irf(var.2c, impulse = "e",
response = c("prod", "rw", "U"), boot = FALSE)
plot(var.2c.irf)
## Prediction
var.2c.prd <- predict(var.2c, n.ahead = 8, ci = 0.95)
plot(var.2c.prd)
## Stability
var.2c.stabil <- stability(var.2c, type = "Rec-CUSUM")
plot(var.2c.stabil)
