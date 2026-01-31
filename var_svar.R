### VAR: Package vars ### 
### Source:https://cran.r-project.org/web/packages/vars/refman/vars.html ###
library(vars)
data(Canada)
head(Canada)
### prod :=	100*(ln(CAN1008S1/445241K)-ln(445005DSA))    Canadian real GDP ###
### e :=	100*ln(445005DSA)                    Canadian civil employment ###
### U :=	444113DSA                           Canadian unemployment rate ###
### rw :=	100*ln(100*444321KSA)         Canadian manufacturing real wage ###
var.2c <- VAR(Canada, p = 2, type = "const")
Acoef(var.2c)
BQ(var.2c)
Bcoef(var.2c)
Phi(var.2c, nstep=4)
Psi(var.2c, nstep=4)

amat <- diag(4)
diag(amat) <- NA
amat[2, 1] <- NA
amat[4, 1] <- NA
## Estimation method scoring
SVAR(x = var.2c, estmethod = "scoring", Amat = amat, Bmat = NULL,
max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 
## Estimation method direct
SVAR(x = var.2c, estmethod = "direct", Amat = amat, Bmat = NULL,
hessian = TRUE, method="BFGS")
 
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace",
              ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
SR
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
LR
SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = FALSE)


VAR(Canada, p = 2, type = "none")
VAR(Canada, p = 2, type = "const")
VAR(Canada, p = 2, type = "trend")
VAR(Canada, p = 2, type = "both")
VARselect(Canada, lag.max = 5, type="const")
arch.test(var.2c)
causality(var.2c, cause = "e")
#use a robust HC variance-covariance matrix for the Granger test:
causality(var.2c, cause = "e", vcov.=vcovHC(var.2c))
#use a wild-bootstrap procedure to for the Granger test
## Not run: causality(var.2c, cause = "e", boot=TRUE, boot.runs=1000)
coef(var.2c)
### Prediction ###
var.2c.prd <- predict(var.2c, n.ahead = 8, ci = 0.95)
fanchart(var.2c.prd)
fevd(var.2c, n.ahead = 5)
fitted(var.2c)

## For VAR
var.2c <- VAR(Canada, p = 2, type = "const")
irf(var.2c, impulse = "e", response = c("prod", "rw", "U"), boot =
FALSE)
## For SVAR
amat <- diag(4)
diag(amat) <- NA
svar.a <- SVAR(var.2c, estmethod = "direct", Amat = amat)
irf(svar.a, impulse = "e", response = c("prod", "rw", "U"), boot =
FALSE)

logLik(var.2c) 
normality.test(var.2c)
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

predict(var.2c, n.ahead = 8, ci = 0.95)
resid(var.2c)
## Restrictions determined by thresh
restrict(var.2c, method = "ser")
## Restrictions set manually
restrict <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0, 
                     1, 0, 1, 0, 0, 1, 0, 1, 1,
                     0, 0, 1, 1, 0, 1, 0, 0, 1,
                     1, 1, 1, 0, 1, 1, 0, 1, 0),
                   nrow=4, ncol=9, byrow=TRUE)
restrict(var.2c, method = "man", resmat = restrict)

roots(var.2c)
serial.test(var.2c, lags.pt = 16, type = "PT.adjusted")
### Stability test ###
var.2c.stabil <- stability(var.2c, type = "OLS-CUSUM")
var.2c.stabil
plot(var.2c.stabil)

## summary-method for varest
var.2c <- VAR(Canada, p = 2 , type = "const")
summary(var.2c)
## summary-method for svarest
amat <- diag(4)
diag(amat) <- NA
amat[2, 1] <- NA
amat[4, 1] <- NA
## Estimation method scoring
svar.a <- SVAR(x = var.2c, estmethod = "scoring", Amat = amat, Bmat = NULL,
max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
summary(svar.a)
## summary-method for svecest
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace",
              ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
svec.b <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot =
FALSE)
summary(svec.b) 

library(urca)
data(finland)
sjf <- finland
sjf.vecm <- ca.jo(sjf, ecdet = "none", type = "eigen", K = 2,
spec = "longrun", season = 4)
vec2var(sjf.vecm, r = 2)