### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm609<- read.csv("xm609.csv", header = TRUE)
attach(xm609)
str(xm609)

library("survival")


survreg(Surv(STRIKECENS80)~ 1,xm609,dist='logistic',
                                    scale=0)

survreg(Surv(STRIKEDUR, STRIKEDUR<80) ~ 1, xm609, dist='weibull',
                                    scale=0)


survreg(Surv(STRIKEDUR, STRIKEDUR<80) ~ PROD, xm609, dist='exponential')


### Panel 14 (p.519) 
library(maxLik)
t <- STRIKEDUR
t1<- STRIKECENS80
loglik <- function(theta) log(theta) - theta*t1
exp_c <- maxLik(loglik, start=1 )
summary(exp_c)
  um Likelihood estimation
Newton-Raphson maximisation, 6 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -280.957 
1  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,] 0.029259   0.003716   7.874 3.44e-15 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### Panel 15 (p.519) 
loglik_pro <- function(theta) {
beta0<- theta[1]
beta1<- theta[2]
beta0*PROD + log(beta1) - exp(beta0*PROD)*beta1*t1
}
exp_c_p <- maxLik(loglik_pro, start= c(1,1))
summary(exp_c_p)            
um Likelihood estimation
Newton-Raphson maximisation, 8 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -278.3842 
2  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,] 6.627933   3.069217   2.159   0.0308 *  
[2,] 0.028350   0.003726   7.608 2.77e-14 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
                  
### Panel 16 (p.519)
loglik_w <- function(theta) {
beta0<- theta[1]
beta1<- theta[2]
sum(log(beta0)+log(beta1)+(beta0-1)*log(t1)) - sum(beta1*t1^(beta0))
}
c <- maxLik(loglik_w, start= c(beta=1,beta1=1))
summary(c)
um Likelihood estimation
Newton-Raphson maximisation, 13 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -280.4292 
2  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
beta  1.115960   0.116574   9.573  <2e-16 ***
beta1 0.018617   0.008835   2.107  0.0351 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.


### Panel 17 (p.519)
loglik_w_pro <- function(theta) {
beta0<- theta[1]
beta1<- theta[2]
beta2<- theta[3]
sum(beta2*PROD+ log(beta0)+log(beta1)+(beta0-1)*log(t1)) - sum(exp(beta2*PROD)*beta1*t1^(beta0))
}
d <- maxLik(loglik_w_pro, start= c(beta=1,beta1=1,beta2=0))
summary(d)
