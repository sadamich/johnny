### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 6 2 (p.451): Example 6 3 (p.457)                               ###
xm601<- read.csv("xm601.csv", header = TRUE)
attach(xm601)
str(xm601)
detach(xm601)
AGE_2<- AGE^2/100

### Panel 1 OLS (p.489)                                                    ###
y<- log(1+INVEST)[log(1+INVEST)>0]
eq<- lm(y~GENDER[log(1+INVEST)>0]+ACTIVITY[log(1+INVEST)>0]
       +AGE[log(1+INVEST)>0]+AGE_2[log(1+INVEST)>0])
summary(eq)
Call:
lm(formula = y ~ GENDER[log(1 + INVEST) > 0] + ACTIVITY[log(1 + 
    INVEST) > 0] + AGE[log(1 + INVEST) > 0] + AGE_2[log(1 + INVEST) > 
    0])
Residuals:
    Min      1Q  Median      3Q     Max 
-2.4936 -0.6527 -0.0191  0.6210  3.3399 
Coefficients:
                              Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    2.85424    0.61133   4.669 3.97e-06 ***
GENDER[log(1 + INVEST) > 0]   -0.21403    0.11579  -1.848  0.06516 .  
ACTIVITY[log(1 + INVEST) > 0] -0.13212    0.09967  -1.326  0.18564    
AGE[log(1 + INVEST) > 0]       0.06978    0.02490   2.802  0.00529 ** 
AGE_2[log(1 + INVEST) > 0]    -0.05593    0.02416  -2.315  0.02103 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9443 on 465 degrees of freedom
  (30 observations deleted due to missingness)
Multiple R-squared:  0.04784,   Adjusted R-squared:  0.03965 
F-statistic: 5.841 on 4 and 465 DF,  p-value: 0.0001361





library(maxLik)
### log(L)= -n/2*log(2*pi)-n/2*log(sigma^2)-1/(2*sigma^2)*sum(y-mu)^2
###            -sum(log(pnorm(mu)/sigma))
### (6 29) (p.487)

l<- function(beta){
beta1<- beta[1]
beta2<- beta[2]
beta3<- beta[3]
beta4<- beta[4]
beta5<- beta[5]
sigma<- beta[6]
y<- log(1+INVEST)
y1<- y[INVEST>0]
n<- 470
mu<- (beta1+ beta2*GENDER[y>0] +beta3*ACTIVITY[y>0]
      +beta4*AGE[y>0]+beta5*AGE_2[y>0])

-n/2*log(2*pi) -n/2*log(sigma^2) -1/(2*sigma^2)*sum((y1-mu)^2)
            -sum(log(pnorm(mu/sigma,0,1)))
}
m<- maxLik(l, start= c(1,1,1,1,1,1))
summary(m)
????


library(truncreg)
d<- xm601
yt <- ifelse(y> 1, y, NA)
AGE_2<- AGE^2
eq_trunc<- truncreg(yt~GENDER+ACTIVITY+AGE+AGE_2, data=xm601,point=0,direction = "left")
summary(eq_trunc)
Call:
truncreg(formula = yt ~ GENDER + ACTIVITY + AGE + AGE_2, data = xm601, 
    point = 0, direction = "left")

BFGS maximization method
15 iterations, 0h:0m:0s 
g'(-H)^-1g = 0.0269 

Coefficients :
               Estimate  Std. Error t-value  Pr(>|t|)    
(Intercept)  2.85424294  0.61136293  4.6687 3.032e-06 ***
GENDER      -0.21402944  0.11578638 -1.8485  0.064532 .  
ACTIVITY    -0.13212190  0.09967440 -1.3255  0.184994    
AGE          0.06978227  0.02490389  2.8021  0.005078 ** 
AGE_2       -0.00055928  0.00024157 -2.3152  0.020603 *  
sigma        0.94425566  0.03105003 30.4108 < 2.2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -637.44 on 6 Df




plot(yt)
hist(yt)
