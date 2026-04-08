### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm520<- read.csv("xm520.csv", header=TRUE)
str(xm520)
attach(xm520)
detach(xm520)
### Problem a (p.434)                                                      ###
TOTCONS_sq<- TOTCONS^2
AHSIZE_sq<- AHSIZE^2
eq<- lm(FOODCONS ~ TOTCONS+AHSIZE+TOTCONS_sq)
summary(eq)
Call:
lm(formula = FOODCONS ~ TOTCONS + AHSIZE + TOTCONS_sq)

Residuals:
       Min         1Q     Median         3Q        Max 
-0.0226391 -0.0033960  0.0002549  0.0031723  0.0161654 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.0024159  0.0045008  -0.537 0.594136    
TOTCONS      0.2834013  0.0159452  17.773  < 2e-16 ***
AHSIZE       0.0075962  0.0006162  12.327 7.22e-16 ***
TOTCONS_sq  -0.0515114  0.0120863  -4.262 0.000105 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.007788 on 44 degrees of freedom
Multiple R-squared:  0.9875,    Adjusted R-squared:  0.9866 
F-statistic:  1156 on 3 and 44 DF,  p-value: < 2.2e-16

### Problem b                                                              ###

library(strucchange)
eq_cusum<- efp(FOODCONS ~ TOTCONS + AHSIZE,type = "Rec-CUSUM")
plot(eq_cusum)
sctest(eq_cusum)
Recursive CUSUM test
data:  eq_cusum
S = 0.63239, p-value = 0.3541

eq_ols<- lm(FOODCONS ~ TOTCONS + AHSIZE)
res_ols<- resid(eq_ols)
plot(res_ols, type="l")