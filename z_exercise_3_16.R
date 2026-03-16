### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv", header =TRUE)
str(xm301)
attach(xm301)
h0<- lm(LOGSAL ~ GENDER)
summary(h0)
### H0 model Call:lm(formula = LOGSAL ~ GENDER)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.65878 -0.23127 -0.07318  0.17106  1.26842 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 10.13246    0.02316  437.43   <2e-16 ***
GENDER       0.41215    0.03140   13.13   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3404 on 472 degrees of freedom
Multiple R-squared:  0.2674,    Adjusted R-squared:  0.2659 
F-statistic: 172.3 on 1 and 472 DF,  p-value: < 2.2e-16
res0<- resid(h0)
h1<- lm(LOGSAL ~ GENDER + EDUC)
summary(h1)
### H1 model Call:lm(formula = LOGSAL ~ GENDER + EDUC)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.62421 -0.18000 -0.01467  0.15515  0.89894 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 9.132275   0.057991 157.476   <2e-16 ***
GENDER      0.245606   0.025817   9.513   <2e-16 ***
EDUC        0.080853   0.004462  18.122   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2616 on 471 degrees of freedom
Multiple R-squared:  0.5684,    Adjusted R-squared:  0.5666 
F-statistic: 310.1 on 2 and 471 DF,  p-value: < 2.2e-16
res1<- resid(h1)
eq_partial<- lm(res0 ~ res1)
summary(eq_partial)

Call:lm(formula = res0 ~ res1)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.51990 -0.03479 -0.02995  0.12692  0.53119 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -3.315e-17  1.002e-02    0.00        1    
res1         1.000e+00  3.843e-02   26.02   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2182 on 472 degrees of freedom
Multiple R-squared:  0.5892,    Adjusted R-squared:  0.5883 
F-statistic:   677 on 1 and 472 DF,  p-value: < 2.2e-16
plot(res0, res1)

eq_g<- lm(GENDER ~ -1)
summary(eq_g)
res_g<- resid(eq_g)
eq_sal <- lm(LOGSAL ~ -1)
summary(eq_sal)
res_sal <- resid(eq_sal)
eq_sal_g<- lm(res_sal ~ res_g)
summary(eq_sal_g)
plot(res_sal,res_g)
### The residuals model coincide with h0. Call:lm(formula = res_sal ~ res_g)###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.65878 -0.23127 -0.07318  0.17106  1.26842 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 10.13246    0.02316  437.43   <2e-16 ***
res_g        0.41215    0.03140   13.13   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.3404 on 472 degrees of freedom
Multiple R-squared:  0.2674,    Adjusted R-squared:  0.2659 
F-statistic: 172.3 on 1 and 472 DF,  p-value: < 2.2e-16
fit<- fitted(eq_sal_g)
lines(fit, col="red")
plot(LOGSAL, GENDER)