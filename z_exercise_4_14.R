### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr414<- read.csv("xr414.csv", header =TRUE)
str(xr414)
attach(xr414)

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

