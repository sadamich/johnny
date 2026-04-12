### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr526<- read.csv("xr526.csv",header =TRUE)
str(xr526)
attach(xr526)
detach(xr526)
y<- log(QUANTITY)
### Problem a (p.434)                                                      ###
AP<- A*DP
eq<- lm(y~ A+AP)
summary(eq)

eq_h0<- lm(y~ DP+A+AP)
summary(eq_h0)

Call:lm(formula = y ~ DP + A + AP)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.147432 -0.015647  0.002297  0.021424  0.128738 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.17571    0.04214 146.567 5.25e-15 ***
DP           0.28084    0.05959   4.713  0.00152 ** 
A            0.18866    0.05959   3.166  0.01327 *  
AP           0.28032    0.08427   3.326  0.01044 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07298 on 8 degrees of freedom
Multiple R-squared:  0.9555,    Adjusted R-squared:  0.9388 
F-statistic: 57.26 on 3 and 8 DF,  p-value: 9.473e-06
### beta2 is significance. H0 is rejected.                                 ###

### Problem b                                                              ###
 A
    [1] 0 0 0 0 0 0 1 1 1 1 1 1
A2<- c(1,1,1,1,1,1,0,0,0,0,0,0)
AP2<- A2*DP
eq_a2<- lm(y ~ DP+A2+AP2)
summary(eq_a2)
Call:lm(formula = y ~ DP + A2 + AP2)
Residuals:
      Min        1Q    Median        3Q       Max 
-0.147432 -0.015647  0.002297  0.021424  0.128738 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.36437    0.04214 151.044 4.13e-15 ***
DP           0.56116    0.05959   9.417 1.33e-05 ***
A2          -0.18866    0.05959  -3.166   0.0133 *  
AP2         -0.28032    0.08427  -3.326   0.0104 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07298 on 8 degrees of freedom
Multiple R-squared:  0.9555,    Adjusted R-squared:  0.9388 
F-statistic: 57.26 on 3 and 8 DF,  p-value: 9.473e-06
### The coefficient of beta2 is larger.                                    ###

### Problem d (p.434)                                                      ###
 6.17571/6.36437
[1] 0.9703568
 0.28084/0.56116
[1] 0.5004633
 0.18866/-0.18866 
[1] -1
 0.28032/-0.28032 
[1] -1
