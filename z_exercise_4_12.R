### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 12
x<- runif(50, 0,2)
eta<- rt(50,3)
y<- 1/2+x+eta
### Problem (a) OLS                                                        ###
eq<- lm(y ~ x)
summary(eq) 
Call:lm(formula = y ~ x)
Residuals:
    Min      1Q  Median      3Q     Max 
-5.8207 -1.0058 -0.1308  0.7127  5.4992 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.05329    0.47804   0.111    0.912   
x            1.28580    0.42517   3.024    0.004 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.772 on 48 degrees of freedom
Multiple R-squared:   0.16,     Adjusted R-squared:  0.1425 
F-statistic: 9.146 on 1 and 48 DF,  p-value: 0.003995
### Problem (b) residuals and its histgram                                 ###
res<- resid(eq)
plot(res)
hist(res)

### Problem (c) GMM 
library("gmm")
sandwich(eq)
(Intercept)           x
(Intercept)  0.04319321 -0.03671665
x           -0.03671665  0.04680081
sqrt(0.04319321)
[1] 0.2078298       GMM standard errors
sqrt(0.04680081)
[1] 0.2163349       GMM standard errors

eq_gmm<- gmm(y~x, x=x)
summary(eq_gmm)
Call:gmm(g = y ~ x, x = x)
Method:  twoStep 
Kernel:  Quadratic Spectral
Coefficients:
             Estimate    Std. Error  t value     Pr(>|t|)  
(Intercept)  2.3604e-01  3.2976e-01  7.1579e-01  4.7412e-01
x            1.3505e+00  2.6340e-01  5.1270e+00  2.9440e-07
J-Test: degrees of freedom is 0 
                J-test                P-value             
Test E(g)=0:    3.58523799913339e-30  *******             

### Problem (d) ML cauchy version                                          ###
library(maxLik)
f_cauchy<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
N<- 50
mu<- beta1+beta2*x
e<- y - mu
 -N*log(pi) -N*log(1+e^2)
}
m2<- maxLik(f_cauchy,start=c(0,1))
summary(m2)
Maximum Likelihood estimation
Newton-Raphson maximisation, 4 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -4699.153 
2  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,]  0.72261    0.05103   14.16  <2e-16 ***
[2,]  0.84680    0.04660   18.17  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------


### Problem (e) ML t5 version
f_t5<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
N<- 50
mu<- beta1+beta2*x
e<- y - mu
-3*N*log(1+1/5*e^2)
}
m3<- maxLik(f_t5,start=c(0,1))
summary(m3)
Maximum Likelihood estimation
Newton-Raphson maximisation, 3 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -1990.099 
2  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,]  0.49484    0.04818   10.27  <2e-16 ***
[2,]  1.06592    0.04121   25.87  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------

### Problem (f)

f_t3<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
N<- 50
mu<- beta1+beta2*x
e<- y - mu
-2*sum(log(1+1/3*e^2))
}
m4<- maxLik(f_t3,start=c(0,1))
summary(m4)
Maximum Likelihood estimation
Newton-Raphson maximisation, 4 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: -37.91255 
2  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]   0.5620     0.3507   1.603 0.108963    
[2,]   0.9989     0.3030   3.297 0.000978 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------

### Problem (g)

GMM is the moment conditions. Moment conditions ???