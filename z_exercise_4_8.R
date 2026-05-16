### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 8 (p. 269)                                                  ###

Multicolinearity
Ommited bias
correlation(x2, x3)> 0

x2<- c(3,-11,-2,5,-19,8,3,2,-3,-6)
x3<- x2+4
e<- rnorm(10, 0,1)
plot(x2,x3)
y<- 1+ x2+x3+e

eq<- lm(y ~ x2+x3)
summary(eq)
### the unrestricted Model                                                ###
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

eq_r<- lm(y ~ x2)
summary(eq_r)
### the omitted varable model                                             ###
lm(formula = y ~ x2)
Residuals:
    Min      1Q  Median      3Q     Max 
-1.7847 -0.3465  0.1927  0.5385  0.7798 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.83892    0.26875   18.01 9.29e-08 ***
x2           1.96789    0.03354   58.67 7.91e-12 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.8229 on 8 degrees of freedom
Multiple R-squared:  0.9977,    Adjusted R-squared:  0.9974 
F-statistic:  3442 on 1 and 8 DF,  p-value: 7.91e-12
res_r<- resid(eq_r)
plot(x2,res_r)


eq_x3<- lm(y~x3)
summary(eq_x3)
res_y<- resid(eq_x3)

eq_x3x2<- lm(x2~x3)
summary(eq_x3x2)
res_x2<- resid(eq_x3x2)

eq_c<- lm(res_y~res_x2)
summary(eq_c)