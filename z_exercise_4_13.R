### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 13  (p.271)                                                 ###
xr413<- read.csv("xr413.csv", header =TRUE)
str(xr413)
attach(xr413)
y<- log(QUANTITY)
x<- log(DEAL)

eqA<- lm(y ~ x)
summary(eqA)
### Linear Model OLS: lm(formula = y ~ x)                                 ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.18705 -0.06228 -0.01716  0.05636  0.23501 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.40656    0.04305  102.36  < 2e-16 ***
x            6.00330    0.58160   10.32 1.19e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.115 on 10 degrees of freedom
Multiple R-squared:  0.9142,    Adjusted R-squared:  0.9056 
F-statistic: 106.5 on 1 and 10 DF,  p-value: 1.188e-06

### Linear Model ML                                                       ###
f<- function(theta){
beta1 = theta[1]
beta2 = theta[2]
sigma = theta[3]
N <- 12
e<- y - beta1 - beta2*x
-0.5*N*log(2*pi) - 0.5*N*log(sigma^2) - 0.5*sum(e^2/sigma^2)
}
m<- maxLik(f, start = c(0,1,1))
summary(m)
Maximum Likelihood estimation
Newton-Raphson maximisation, 9 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: 10.02358 
3  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  4.40656    0.03930 112.135  < 2e-16 ***
[2,]  6.00330    0.53090  11.308  < 2e-16 ***
[3,]  0.10495    0.02142   4.899 9.64e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### ML = OLS                                                               ###

f2<- function(theta){
c = theta[1]
d = theta[2]
sigma = theta[3]
N <- 12
e<- y - c*(1+d*x)
-0.5*N*log(2*pi) - 0.5*N*log(sigma^2) - 0.5*sum(e^2/sigma^2)
}
m2<- maxLik(f2, start = c(0,1,1))
summary(m2)
Maximum Likelihood estimation
Newton-Raphson maximisation, 8 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: 10.02358 
3  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  4.40656    0.03930 112.123  < 2e-16 ***
[2,]  1.36235    0.12859  10.594  < 2e-16 ***
[3,]  0.10495    0.02142   4.899 9.64e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
4.40656*  1.36235       ### c * d                     
[1] 6.003277            ### b = c * d

### Wald test w = n/n-k *t^2
(1.36235 - 1) /0.12859
[1] 2.817871
12/10*2.817871^2
[1] 9.528476
1 - pchisq(9.528476,1)
[1] 0.00202308
### H0 (d = 1) is rejected.

### LM test  

res<- y -  4.40656*(1+1.36235*x)
eq_aux<- lm(res ~ x)
summary(eq_aux)
Call:
lm(formula = res ~ x)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.18705 -0.06228 -0.01716  0.05636  0.23501 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)
(Intercept) 5.630e-07  4.305e-02       0        1
x           2.062e-05  5.816e-01       0        1
Residual standard error: 0.115 on 10 degrees of freedom
Multiple R-squared:  1.257e-10, Adjusted R-squared:   -0.1 
F-statistic: 1.257e-09 on 1 and 10 DF,  p-value: 1
12* 1.257e-10
[1] 1.5084e-09
### H0 (d=1)  the restricted model                                         ###
f3<- function(theta){
c = theta[1]
sigma = theta[2]
N <- 12
e<- y - c*(1+x)
-0.5*N*log(2*pi) - 0.5*N*log(sigma^2) - 0.5*sum(e^2/sigma^2)
}
m3<- maxLik(f3, start = c(0,1))
summary(m3)
--------------------------------------------
Maximum Likelihood estimation
Newton-Raphson maximisation, 7 iterations
Return code 1: gradient close to zero (gradtol)
Log-Likelihood: 6.891671 
2  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  4.48295    0.03751 119.525  < 2e-16 ***
[2,]  0.13625    0.02781   4.899 9.64e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
res0<- y - 4.48295*(1+x)
eq_res0<- lm(res0 ~ x)
summary(eq_res0)
Call:
lm(formula = res0 ~ x)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.18705 -0.06228 -0.01716  0.05636  0.23501 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.07639    0.04305  -1.775   0.1064  
x            1.52035    0.58160   2.614   0.0259 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 0.115 on 10 degrees of freedom
Multiple R-squared:  0.4059,    Adjusted R-squared:  0.3465 
F-statistic: 6.833 on 1 and 10 DF,  p-value: 0.02586
12* 0.4059
[1] 4.8708
1 - pchisq( 4.8708,1)
[1] 0.02731483      (P value) 
### LR test
 2*10.02358 -2* 6.891671 
[1] 6.263818
1 - pchisq(6.263818,1)
[1] 0.01232284    (P value) 


### 0.02731483 (LM) < 0.01232284 (LR) < 0.00202308(W) P values comparision ###              
