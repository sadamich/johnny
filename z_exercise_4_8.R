### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
Exercise 4 8

Multicolinearity

Ommited bias

Simulation

correlation(x2, x3)> 0

x2<- rnorm(10, 0, 1)
x3<- x2+4
e<- rnorm(10, 0,1)
plot(x2,x3)
y<- 1+ x2 + x3
plot(y)
hist(y)
eq<- lm(y ~ x2+x3)
summary(eq)

Call:
lm(formula = y ~ x2 + x3)

Residuals:
       Min         1Q     Median         3Q        Max 
-1.620e-15 -1.315e-16  4.674e-17  1.907e-16  1.176e-15 

Coefficients: (1 not defined because of singularities)
             Estimate Std. Error   t value Pr(>|t|)    
(Intercept) 5.000e+00  2.388e-16 2.094e+16   <2e-16 ***
x2          2.000e+00  2.441e-16 8.193e+15   <2e-16 ***
x3                 NA         NA        NA       NA    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 7.322e-16 on 8 degrees of freedom
Multiple R-squared:      1,     Adjusted R-squared:      1 
F-statistic: 6.712e+31 on 1 and 8 DF,  p-value: < 2.2e-16
Warning message:
In summary.lm(eq) : essentially perfect fit: summary may be unreliable

eq_o<- lm(y ~ x2)
summary(eq_o)
Call:
lm(formula = y ~ x2)

Residuals:
       Min         1Q     Median         3Q        Max 
-1.620e-15 -1.315e-16  4.674e-17  1.907e-16  1.176e-15 

Coefficients:
             Estimate Std. Error   t value Pr(>|t|)    
(Intercept) 5.000e+00  2.388e-16 2.094e+16   <2e-16 ***
x2          2.000e+00  2.441e-16 8.193e+15   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.322e-16 on 8 degrees of freedom
Multiple R-squared:      1,     Adjusted R-squared:      1 
F-statistic: 6.712e+31 on 1 and 8 DF,  p-value: < 2.2e-16

Warning message:
In summary.lm(eq_o) : essentially perfect fit: summary may be unreliable





