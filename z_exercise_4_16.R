### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr416<- read.csv("xr416.csv", header=TRUE)
str(xr416)
attach(xr416)
y<- FOODCONS/TOTCONS
x2<- TOTCONS
x3<- AHSIZE

### Problem a (p.271)                                                      ###
plot(x2,y)
### Non linearity is existent. Therefore, beta3 = 1/2 is plausible.        ###

e<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
beta3<- theta[3]
beta4<- theta[4]
result<- -(y - beta1-beta2*x2^beta3-beta4*x3)^2
return(result)
}

library(maxLik)
eq_opt<- maxNR(e, start= c(0,0,1,0))
summary(eq_opt)
Newton-Raphson maximisation 
Number of iterations: 11 
Return code: 2 
successive function values within tolerance limit (tol) 
Function value: -0.02240811 
Estimates:
        estimate     gradient
[1,]  0.44670932 8.259505e-07
[2,] -0.25082508 4.870489e-07
[3,]  0.42792510 1.272326e-07
[4,]  0.01502752 2.962213e-06
--------------------------------------------

### NLS model                                                              ###library(gslnls)
library(gslnls)
eq_nls<- gsl_nls(fn = y ~ beta1+beta2*x2^beta3+beta4*x3, data = xr416, start = c(beta1 = 0, beta2 = 0, beta3= 1,beta4=0))
summary(eq_nls)
Formula: y ~ beta1 + beta2 * x2^beta3 + beta4 * x3
Parameters:
       Estimate Std. Error t value Pr(>|t|)    
beta1  0.446710   0.083574   5.345 2.23e-06 ***
beta2 -0.250825   0.081218  -3.088  0.00328 ** 
beta3  0.427924   0.201230   2.127  0.03842 *  
beta4  0.015028   0.001416  10.611 2.10e-14 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02117 on 50 degrees of freedom
Number of iterations to convergence: 12 
Achieved convergence tolerance: 9.854e-14
res_nls<- resid(eq_nls)
ssr<- sum(res_nls^2)
ssr
[1] 0.02240811
### The coefficients are similar with ML and NLS models                     ###

### H0 test and P values Problem (d)                                        ###
 (0.427924 - 0.5)/ 0.201230 =  -0.3581772   H0 is not rejectet.
pt(-0.3581772,50)*2
[1] 0.7217185     (P value)
(0.427924 - 0)/ 0.201230   =   2.126542    H0 is rejected.
2*(1- pt(2.126542, 50))
[1] 0.03841688    (P value)
 (0.427924 - 1)/ 0.201230   =  -2.842896    H0 is rejected.
pt( -2.842896,50)*2
[1] 0.006457147

### F test Problem (e) 
F<- function(ssr_r,ssr, g, n, k){
(ssr_r - ssr)/g / (ssr/(n-k))
}
H0 is beta3= 0
eq_0<- lm(y ~ x3)
summary(eq_0)
res_0<- resid(eq_0)
ssr_0<- sum(res_0^2)
ssr_0
[1] 0.1191041
F(0.1191041, 0.02240811,1,54,4)
[1] 215.7611
1-pf(215.7611, 1, 50)
[1] 0                 (P value) H0 is rejected

H0 is beta3 = 1
eq1<- lm(y ~ x2+x3)
summary(eq1)
res_1<- resid(eq1)
ssr_1<- sum(res_1^2)
ssr_1
[1] 0.02581779
F(0.02581779, 0.02240811,1,54,4)
[1] 7.608138
1 - pf(7.608138, 1,50)
[1] 0.008094644

H0 is beta3 = 1/2
eq2<- lm(y ~ sqrt(x2)+x3)
summary(eq2)
res_2<- resid(eq2)
ssr_2<- sum(res_2^2)
ssr_2
[1] 0.02246881
F(0.02246881 ,0.02240811,1,54,4)
[1] 0.135442
1- pf(0.135442,1,50)
[1] 0.7144085    (P value) that is the same with anova.

eq_nls_r<- gsl_nls(fn = y ~ beta1+beta2*x2^(1/2)+beta4*x3, data = xr416, start = c(beta1 = 0, beta2 = 0,beta4=0))
summary(eq_nls_r)
anova(eq_nls, eq_nls_r)
Analysis of Variance Table
Model 1: y ~ beta1 + beta2 * x2^beta3 + beta4 * x3
Model 2: y ~ beta1 + beta2 * x2^(1/2) + beta4 * x3
  Res.Df Res.Sum Sq Df    Sum Sq F value Pr(>F)
1     50   0.022408                            
2     51   0.022469 -1 -6.07e-05  0.1354 0.7144
### H0 (beta3=1/2) is not rejected.                                         ###
### Problem (f) LR test                                                     ###
logLik(eq_nls)
logLik(eq_nls_r)
2* 133.6349 - 2*133.5618 =  0.1462
1- pchisq(0.1462,1)
[1] 0.7021936
### H0 (beta3=1/2) is not rejected.                                         ###
res<- resid(eq_nls_r)
                                  