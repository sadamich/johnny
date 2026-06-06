### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr414<- read.csv("xr414.csv", header =TRUE)
str(xr414)
attach(xr414)

### Problem (a)  ML 
f<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
beta3<- theta[3]
beta4<- theta[4]
beta5<- theta[5]
sigma<- theta[6]
N<- 474
mu<- beta1+ beta2*EDUC+ beta3*LOGSALBEGIN+beta4*GENDER+beta5*MINORITY

-N*0.5*log(2*pi)- N*0.5*log(sigma^2)- 0.5*((LOGSAL - mu)^2/sigma^2)
}
library(maxLik)
m<- maxLik(f, start = c(0,0,0,0,0,1))
summary(m)
Maximum Likelihood estimation
Newton-Raphson maximisation, 11 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: 764082 
6  free parameters
Estimates:
       Estimate Std. error t value Pr(> t)    
[1,]  2.080e+00  1.465e-02  141.94  <2e-16 ***
[2,]  2.327e-02  1.777e-04  130.94  <2e-16 ***
[3,]  8.218e-01  1.677e-03  490.13  <2e-16 ***
[4,]  4.816e-02  9.139e-04   52.70  <2e-16 ***
[5,] -4.237e-02  9.301e-04  -45.55  <2e-16 ***
[6,]  8.069e-03  1.204e-05  670.13  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
res<- LOGSAL - 2.08- 2.327e-02*EDUC-0.8218*LOGSALBEGIN-0.04816*GENDER+0.04237*MINORITY
ssr<- sum(res^2)
ssr
[1] 14.62757
s_sq<- sum(res^2)/472
s_sq
[1] 0.5521511
### Compare with OLS:lm(LOGSAL ~ EDUC + LOGSALBEGIN + GENDER + MINORITY)   ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.45572 -0.11508 -0.00516  0.10765  0.87060 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.07965    0.31480   6.606 1.07e-10 ***
EDUC         0.02327    0.00387   6.013 3.66e-09 ***
LOGSALBEGIN  0.82180    0.03603  22.808  < 2e-16 ***
GENDER       0.04816    0.01991   2.419   0.0160 *  
MINORITY    -0.04237    0.02034  -2.083   0.0378 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1766 on 469 degrees of freedom
Multiple R-squared:  0.8041,    Adjusted R-squared:  0.8024 
F-statistic: 481.3 on 4 and 469 DF,  p-value: < 2.2e-16
### OLS = ML 

### b5 = 0
f1<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
beta3<- theta[3]
beta4<- theta[4]
sigma<- theta[5]
N<- 474
mu<- beta1+ beta2*EDUC+ beta3*LOGSALBEGIN+beta4*GENDER

-N*0.5*log(2*pi)- N*0.5*log(sigma^2)- 0.5*((LOGSAL - mu)^2/sigma^2)
}
library(maxLik)
m1<- maxLik(f1, start = c(0,0,0,0,1))
summary(m1)
--------------------------------------------
Maximum Likelihood estimation
Newton-Raphson maximisation, 29 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: 763047.7 
5  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
[1,] 1.9322810  0.0140108  137.91  <2e-16 ***
[2,] 0.0233781  0.0001774  131.80  <2e-16 ***
[3,] 0.8364063  0.0016145  518.07  <2e-16 ***
[4,] 0.0396000  0.0008931   44.34  <2e-16 ***
[5,] 0.0081060  0.0000121  670.13  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
res1<- LOGSAL - 1.9322810- 0.0233781*EDUC - 0.8364063*LOGSALBEGIN-0.0396000*GENDER
s_sq<- sum(res1^2)/472
s_sq
[1] 0.03127712
ssr1<- sum(res1^2)
ssr1
### b4= b5=0 
f2<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
beta3<- theta[3]
sigma<- theta[4]
N<- 474
mu<- beta1+ beta2*EDUC+ beta3*LOGSALBEGIN

-N*0.5*log(2*pi)- N*0.5*log(sigma^2)- 0.5*((LOGSAL - mu)^2/sigma^2)
}
library(maxLik)
m2<- maxLik(f2, start = c(0,0,0,1))
summary(m2)
Maximum Likelihood estimation
Newton-Raphson maximisation, 11 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: 762071.3 
4  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
[1,] 1.647e+00  1.293e-02   127.4  <2e-16 ***
[2,] 2.312e-02  1.800e-04   128.5  <2e-16 ***
[3,] 8.685e-01  1.498e-03   579.8  <2e-16 ***
[4,] 8.141e-03  1.215e-05   670.1  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
res2<- LOGSAL - 1.647-0.02312*EDUC-0.8685*LOGSALBEGIN-0.008141*GENDER
s_sq2<- sum(res2^2)/472
s_sq2
[1] 0.03147417
ssr2<- sum(res2^2)
ssr2
### b4+b5 =0
f3<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
beta3<- theta[3]
beta4<- theta[4]
sigma<- theta[5]
N<- 474
mu<- beta1+ beta2*EDUC+ beta3*LOGSALBEGIN+beta4*GENDER-beta4*MINORITY

-N*0.5*log(2*pi)- N*0.5*log(sigma^2)- 0.5*((LOGSAL - mu)^2/sigma^2)
}

m3<- maxLik(f3, start = c(1,1,1,1,1))
summary(m3)
Maximum Likelihood estimation
Newton-Raphson maximisation, 12 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: 764069.5 
5  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
[1,] 2.065e+00  1.455e-02  141.99  <2e-16 ***
[2,] 2.324e-02  1.782e-04  130.38  <2e-16 ***
[3,] 8.235e-01  1.659e-03  496.30  <2e-16 ***
[4,] 4.534e-02  7.192e-04   63.04  <2e-16 ***
[5,] 8.069e-03  1.204e-05  670.13  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

res3<- LOGSAL -2.065 -0.02324*EDUC-0.8235*LOGSALBEGIN-0.04534*(GENDER-MINORITY)
s_sq3<- sum(res3^2)/472
s_sq3
[1] 0.03099449
ssr3<- sum(res3^2)
ssr3
[1] 14.6294

### LR test
2*(764082 - 763047.7)
[1] 2068.6
1- pchisq(2068.6,1)
[1] 0

2*(764082 - 762071.3)
[1] 4021.4
1- pchisq(4021.4,1)
[1] 0

2*(764082 +72422.43) 
[1] 1673009
1- pchisq(1673009,1)
[1] 0

### LM test
eq_lm<- lm(res1~ EDUC+LOGSALBEGIN+GENDER)
summary(eq_lm)
474* 2.752e-13
[1] 1.304448e-10
1- pchisq(1.304448e-10,1)
[1] 0.9999909

eq_lm2<- lm(res2~ EDUC+LOGSALBEGIN)
summary(eq_lm2)
474* 0.0001572
[1] 0.0745128
1- pchisq(0.0745128,1)
[1] 0.784876
gm<- GENDER - MINORITY
eq_lm3<- lm(res3~ EDUC+LOGSALBEGIN+ gm)
summary(eq_lm3)
474*  0.9852
[1] 466.9848
1 - pchisq(466.9848,1)
[1] 0

### Wald test  n/(n-k)* t^2
H0 (beta5 = 0)
474/(474-5)*(-45.55)^2
[1] 2096.922
library(car)
H0 (beta4=beta5=0)
not single restriction?
R<- matrix(c(0,0,0,0,0,
             0,0,0,0,0,
             0,0,0,0,0,
             0,0,0,1,0,
             0,0,0,0,1), nrow=5,ncol=5)
R
 [,1] [,2] [,3] [,4] [,5]
[1,]    0    0    0    0    0
[2,]    0    0    0    0    0
[3,]    0    0    0    0    0
[4,]    0    0    0    1    0
[5,]    0    0    0    0    1
r<- rep(0,5)
linearHypothesis(f,R,r,test = "Chisq") ???
### W = gF (p.235)                                                         ###
F<- function(ssr_r,ssr,g,n,k){
result<- ((ssr_r-ssr)/g)/(ssr/(n-k))
return(result)
}
ssr

F(14.85581 , 14.62757  , 2, 474, 5)
[1] 3.659
W = ng/(n-k)F
(474*2)/(474-5)*3.659
[1] 7.396017

H0 (beta4+beta5=0)
not single restirction?
F( 14.6294, 14.62757  , 2, 474, 5)
[1] 0.02933741
(474*2)/(474-5)*0.02933741
[1] 0.05930035
### LM test problem (d)                                                    ###


