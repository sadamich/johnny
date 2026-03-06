### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
vwgh<- read.csv2("vwgh.csv", header =TRUE)
str(vwgh)
attach(vwgh)
hist(dauer1)
hist(dauer2)
hist(dauer3)
vwghdauer <- with(vwgh, dauer3[dauer3 != -9999])
hist(vwghdauer)
### Masszahlen zur Beschreibung der Verteilung, S.309 ###
summary(vwghdauer)
var(vwghdauer)
sd(vwghdauer)
### Min, 1.Quartil, Median, 3.Quartil. Max ###
fivenum(vwghdauer)
### Boxplot                                ###
boxplot(vwghdauer, ylab = "Dauer in Tagen")
### T. Test                         S. 329 ###
t.test(vwghdauer, mu = 456, alternative ="greater")
### Konfidenzintervall              S. 331 ###
t.test(vwghdauer)$conf.int

library(maxLik)
t <- vwghdauer
loglik <- function(theta) log(theta) - theta*t
a <- maxLik(loglik, start=1 )
summary(a)
Maximum Likelihood estimation
Newton-Raphson maximisation, 9 iterations
Return code 2: successive function values within tolerance limit (tol)
Log-Likelihood: -29281.27 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
[1,] 1.093e-03  1.788e-05   61.14  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1

loglik_pro <- function(theta) {
beta0<- theta[1]
beta1<- theta[2]
beta0*senat + log(beta1) - exp(beta0*senat)*beta1*t
}
b <- maxLik(loglik_pro, start= c(0,1))
summary(b)

loglik_w <- function(theta) {
beta0<- theta[1]
beta1<- theta[2]
sum(log(beta0)+log(beta1)+(beta0-1)*log(t)) - sum(beta1*t^(beta0))
}
c <- maxLik(loglik_w, start= c(beta=1,beta1=1))
summary(c)
-------------------------------------------
Maximum Likelihood estimation
Newton-Raphson maximisation, 8 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -29204.82 
2  free parameters
Estimates:
       Estimate Std. error t value  Pr(> t)    
beta  1.1912530  0.0205367  58.006  < 2e-16 ***
beta1 0.0002781  0.0000416   6.685 2.31e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1