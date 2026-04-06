### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr417<- read.csv("xr417.csv", header=TRUE)
str(xr417)
attach(xr417)
### Problem (b) ML based on cauchy distribution                            ###
f_cauchy<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
N<- 240
mu<- beta1+beta2*RENDMARK
-N*log(pi) - N*log(1+(RENDNCCO - mu)^2)
}
m_cauchy<- maxLik(f_cauchy, start = c(0,0))
summary(m_cauchy)
 Maximum Likelihood estimation
Newton-Raphson maximisation, 8 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -148185.8 
2  free parameters
Estimates:
     Estimate Std. error t value Pr(> t)    
[1,] 0.136400   0.007844   17.39  <2e-16 ***
[2,] 0.914571   0.001548  590.94  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1

### Problem (c) ML under normal distribution assumption                    ###
f<- function(theta){
beta1<- theta[1]
beta2<- theta[2]
sigma<- theta[3]
N<- 240
mu<- beta1+beta2*RENDMARK
-N*0.5*log(2*pi)-N*0.5*log(sigma^2)-0.5*((RENDNCCO - mu)^2/sigma^2)
}
library(maxLik)
m<-maxLik(f, start=c(0,0,1))
summary(m)

Maximum Likelihood estimation
Newton-Raphson maximisation, 8 iterations
Return code 2: successive function values within tolerance limit (tol)
Log-Likelihood: 15407.54 
3  free parameters
Estimates:
       Estimate Std. error t value Pr(> t)    
[1,]  0.1978772  0.0121282   16.32  <2e-16 ***
[2,]  0.9315372  0.0025186  369.86  <2e-16 ***
[3,] -0.1851793  0.0005456 -339.42  <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

eps<- rcauchy(n=240, location = 0, scale = 1)

eq_ols<- lm(RENDNCCO ~ RENDMARK)
summary(eq_ols)
### OLS lm(RENDNCCO ~ RENDMARK)=ML(under normal distribution assumption)   ###                      
Residuals:
     Min       1Q   Median       3Q      Max 
-13.0149  -1.5552  -0.0605   1.6777  11.2295 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.19788    0.18864   1.049    0.295    
RENDMARK     0.93154    0.03918  23.775   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 2.881 on 238 degrees of freedom
Multiple R-squared:  0.7037,    Adjusted R-squared:  0.7025 
F-statistic: 565.2 on 1 and 238 DF,  p-value: < 2.2e-16
### Problem (d) t test:H0 is rejected.                                     ###
0.136400   0.007848    17.38  <2e-16 
0.1978772  0.0121282   16.32  <2e-16
### Problem (e) 
(0.914571 -1)/  0.001548 = -55.18669
(0.9315372 -1)/  0.0025186 = -27.18288
### Problem (f) Residuals                                                  ###
f_res_c<- (RENDNCCO -(0.136400 +0.914571*RENDMARK))
hist(f_res_c)
f_res<- (RENDNCCO -(0.19788+0.93154*RENDMARK))
hist(f_res)
library(tseries)
jarque.bera.test(f_res)
Jarque Bera Test
data:  f_res
X-squared = 84.123, df = 2, p-value < 2.2e-16
### f_res is not normal distributet.                                       ###

### Problem (g)                                                            ###
library("gmm")
eq_gmm<- gmm(RENDNCCO~RENDMARK, x=RENDMARK)
summary(eq_gmm)
Call:gmm(g = RENDNCCO ~ RENDMARK, x = RENDMARK)
Method:  twoStep 
Kernel:  Quadratic Spectral
Coefficients:
             Estimate    Std. Error  t value     Pr(>|t|)  
(Intercept)  1.9788e-01  1.7422e-01  1.1358e+00  2.5603e-01
RENDMARK     9.3154e-01  4.4034e-02  2.1155e+01  2.4894e-99
J-Test: degrees of freedom is 0 
                J-test                P-value             
Test E(g)=0:    2.95041575704312e-27  *******   
