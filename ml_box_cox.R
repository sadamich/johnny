### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 5 5 Bank wages (p.300)                                         ###

xm501<- read.csv("xm501.csv",header =TRUE)
str(xm501)
attach(xm501)
### OLS (lambda = 1)                                                       ###
S<- SALARY/10000
eq_ols<- lm(S ~ GENDER+MINORITY+EDUC)
summary(eq_ols)
### Call:lm(formula = S ~ GENDER + MINORITY + EDUC)                        ###
Residuals:
    Min      1Q  Median      3Q     Max 
-2.3267 -0.7345 -0.1926  0.4794  7.7404 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.33133    0.27634  -4.818 1.96e-06 ***
GENDER       0.90222    0.12012   7.511 2.98e-13 ***
MINORITY    -0.51168    0.13630  -3.754 0.000196 ***
EDUC         0.32572    0.02089  15.596  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.206 on 470 degrees of freedom
Multiple R-squared:  0.504,     Adjusted R-squared:  0.5009 
F-statistic: 159.2 on 3 and 470 DF,  p-value: < 2.2e-16

l_linear<- function(theta) {
   beta<- theta[1]
   beta2<- theta[2]
   beta3<- theta[3]
   beta4<- theta[4]                        
   sigma <- theta[5]
   S<- SALARY/10000
   N <- 474
   mu<- beta+ beta2*GENDER + beta3*MINORITY+ beta4*EDUC
   -0.5*N*log(2*pi) - 0.5*N*log(sigma^2) 
          -sum(0.5*(S - mu)^2/sigma^2)                           
}

library(maxLik)
m<- maxLik(l_linear, start=c(beta=-1.3,beta2=0.9,beta3=-0.5,beta4=0.32, sigma=0))
summary(m)
### Loglinear model(lambda=0)                                               ###
eq_log<- lm(log(S) ~ GENDER+ MINORITY+ EDUC)
summary(eq_log)
Call: lm(formula = log(S) ~ GENDER + MINORITY + EDUC)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.65514 -0.17437 -0.02399  0.15191  0.94663 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.010360   0.058687  -0.177     0.86    
GENDER       0.261131   0.025511  10.236  < 2e-16 ***
MINORITY    -0.132673   0.028946  -4.583 5.87e-06 ***
EDUC         0.077366   0.004436  17.442  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2562 on 470 degrees of freedom
Multiple R-squared:  0.5869,    Adjusted R-squared:  0.5842 
F-statistic: 222.5 on 3 and 470 DF,  p-value: < 2.2e-16


loglik <- function(theta) {
   beta<- theta[1]
   beta2<- theta[2]
   beta3<- theta[3]
   beta4<- theta[4]                        
   lambda<- theta[5]
   sigma <- theta[6]
   S<- SALARY/10000
   y_lam<- (S^lambda - 1)/lambda
   EDUC_lam <- (EDUC^lambda -1)/lambda
   GENDER_lam <- (GENDER^lambda -1)/lambda
   MINORITY_lam <- (MINORITY^lambda -1)/lambda
   mu<- (beta^lambda -1)/lambda + beta2*GENDER_lam + beta3*MINORITY_lam + 
         beta4*EDUC_lam
   ll <- -0.5*N*log(2*pi) - 0.5*N*log(sigma^2) 
          -(lambda -1)*sum(log(S))- sum(0.5*(y_lam - mu)^2/sigma^2)                           
}
N <- 474
library(maxLik)
m<- maxLik(loglik, start=c(0,0,0,0,0,0))
summary(m)
???
 -0.5*N*log(2*pi)- N*log(sigma) + (lambda -1)*sum(log(S)) 
- sum(0.5*(y- mu)^2/sigma^2)
