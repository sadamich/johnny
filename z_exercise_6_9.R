### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 6 9 (p. 527)                                                  ###
set.seed(69)
x<- rnorm(10,100,100)
e<- rnorm(10, 0, 1)
y<- -10+0.1*x+e
data<- data.frame(x,e,y)
str(data)
str(y)
plot(y)
hist(y)
y_c<- ifelse(y < 0, 0, 1)
y_0<- y[y_c<=0]
str(y_0)
y_1<- y[y_c>0]
str(y_1)
### OlS
eq<- lm(y~x)
summary(eq)
### Call:lm(formula = y ~ x)
Residuals:
     Min       1Q   Median       3Q      Max 
-1.05997 -0.41224  0.02539  0.55412  0.74738 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -10.255217   0.268017  -38.26 2.39e-10 ***
x             0.100637   0.002216   45.41 6.10e-11 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.6801 on 8 degrees of freedom
Multiple R-squared:  0.9961,    Adjusted R-squared:  0.9957 
F-statistic:  2063 on 1 and 8 DF,  p-value: 6.103e-11

### Problem (a) The truncated model: ML estimate 
library(truncreg)
y_c<- ifelse(y > 0,1,0)
x<- as.vector(x)
e<- as.vector(e)
y<- as.vector(y)
data<- data.frame(x,e)
eq_trunc<- truncreg(y_c~x,data=data,point=0,direction = "left")
summary(eq_trunc)
### Call:truncreg(formula = y_c ~ x, data = data, point = 0, direction = "left")
BFGS maximization method
55 iterations, 0h:0m:0s 
g'(-H)^-1g = 1.59E-07 
Coefficients :
              Estimate Std. Error t-value  Pr(>|t|)    
(Intercept) -0.6248293  0.4567904 -1.3679 0.1713532    
x            0.0081820  0.0023399  3.4968 0.0004709 ***
sigma        0.2649894  0.0839400  3.1569 0.0015946 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: 8.273 on 3 Df


### Problem (b) Comparision with OLS (bias), the marginal effects             ###
### Truncated bias (p.486)

### Problem (c) The censored model
library("censReg")
eq_cen<- censReg(y~x)
summary(eq_cen)
### censReg(formula = y ~ x)
Observations:
         Total  Left-censored     Uncensored Right-censored 
            10              6              4              0 
Coefficients:
             Estimate Std. error t value Pr(> t)    
(Intercept) -9.659681   0.675715 -14.295  <2e-16 ***
x            0.098125   0.003636  26.989  <2e-16 ***
logSigma    -0.893438   0.353553  -2.527  0.0115 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Newton-Raphson maximisation, 5 iterations
Return code 1: gradient close to zero (gradtol)
Log-likelihood: -2.102004 on 3 Df
### Problem (d) Comparision with OLS (bias), 
### Censored bias (p.493)

### Problem (e) Comparision of the truncated model with the censored



