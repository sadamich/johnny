### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr417<- read.csv("xr417.csv", header=TRUE)
str(xr417)
attach(xr417)

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