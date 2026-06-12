### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 10 (p.269)                                                  ###
set.seed(45)
x<- rnorm(100, 0, 1)
x_sq<- x^2
w<- rnorm(100, 0, 1)
y<- 1+x_sq+w
plot(x, y)
plot(x_sq,y)
eq<- lm(y ~ x+x_sq)
summary(eq)
### the unrestricted Model :lm(formula = y ~ x + x_sq)                      ###
Residuals:
    Min      1Q  Median      3Q     Max 
-2.3091 -0.7772  0.0184  0.6431  3.1875 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.27944    0.12029  10.636   <2e-16 ***
x           -0.03823    0.11005  -0.347    0.729    
x_sq         1.06256    0.08200  12.958   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9871 on 97 degrees of freedom
Multiple R-squared:  0.6477,    Adjusted R-squared:  0.6404 
F-statistic: 89.17 on 2 and 97 DF,  p-value: < 2.2e-16

eq_r<- lm(y~ x_sq)
summary(eq_r)
### The restricted Model :lm(formula = y ~ x_sq)
Residuals:
    Min      1Q  Median      3Q     Max 
-2.3328 -0.7215  0.0178  0.6066  3.2006 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.27511    0.11911   10.71   <2e-16 ***
x_sq         1.06873    0.07969   13.41   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9826 on 98 degrees of freedom
Multiple R-squared:  0.6473,    Adjusted R-squared:  0.6437 
F-statistic: 179.8 on 1 and 98 DF,  p-value: < 2.2e-16
res_r<- resid(eq_r)
z<- 2*x
eq_aux<- lm(res_r ~ z)
summary(eq_aux)
Call:lm(formula = res_r ~ z)
Residuals:
     Min       1Q   Median       3Q      Max 
-2.66482 -0.69262  0.09193  0.76852  2.37461 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  0.00346    0.10187   0.034    0.973
z           -0.02131    0.04502  -0.473    0.637
Residual standard error: 1.016 on 98 degrees of freedom
Multiple R-squared:  0.00228,   Adjusted R-squared:  -0.007901 
F-statistic: 0.224 on 1 and 98 DF,  p-value: 0.6371
0.00228*100
LM = 100*0.00228=  0.228
### H0 (b2=0) is not rejected.
no_sample<- c(10, 100)
eq<- lm(y~ x+x_sq)
for (i in 1:1000)???
eq<- lm(y ~ x+x_sq)
eq_r<- lm(y~ x_sq)
res_r<- resid(eq_r)
eq_aux<- lm(res_r ~ x+x_sq)


